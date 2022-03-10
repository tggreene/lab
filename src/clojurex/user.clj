(ns user)

;; Setting up a good clojure dev session

(defn log-missing-ns
  [ns]
  (println (format "Couldn't load %s, maybe missing dependency?" ns)))

(defn try-require
  [ns]
  (try
    (require ns)
    ns
    (catch Exception _
      (log-missing-ns ns))))

(defn load-debugging!
  [cljs?]
  (and (try-require 'hashp.core)
       (try-require 'debux.core)
       (if cljs? (try-require 'hashc.core) true)
       :ok))

(defn load-classpath-watcher!
  [aliases]
  (when (try-require 'lambdaisland.classpath.watch-deps)
    (let [start! (resolve 'lambdaisland.classpath.watch-deps/start!)]
      (future
        (try
          (println "Starting classpath-watcher" aliases)
          (start! {:aliases aliases})
          (catch Exception e
            (println "Failed to start classpath-watcher")
            (prn e))))
      :ok)))

(def deps-watcher (atom nil))
(defn start-watch-deps! [_opts] (println "You need to install-better-deps-watcher!"))
(defn stop-watch-deps! [] (println "You need to install-better-deps-watcher!"))

(defn install-better-deps-watcher!
  []
  (when (and (try-require 'hawk.core)
             (try-require 'lambdaisland.classpath)
             (try-require 'clojure.tools.deps.alpha))
    (let [install-priority-loader! (resolve 'lambdaisland.classpath/install-priority-loader!)
          system-classpath (resolve 'clojure.java.classpath/system-classpath)
          create-basis (resolve 'clojure.tools.deps.alpha/create-basis)
          hawk-watch! (resolve 'hawk.core/watch!)
          hawk-stop! (resolve 'hawk.core/stop!)
          handler
          (fn handler
            [opts _ {:keys [kind]}]
            (when (= :modify kind)
              (println "✨ Reloading deps.edn ✨")
              (let [new-paths (remove (set (map str (system-classpath)))
                                      (:classpath-roots (create-basis opts)))]
                (doseq [path new-paths]
                  (println "- " path))
                (install-priority-loader! new-paths)))) ]
      (alter-var-root
       #'start-watch-deps!
       (constantly
        (fn start-watch-deps!
          [opts]
          (println "Starting watch-deps" opts)
          (swap! deps-watcher
                 (fn [watcher]
                   (when watcher
                     (println "Stopping existing deps.edn watcher")
                     (hawk-stop! watcher))
                   (hawk-watch! [{:paths ["deps.edn"]
                                  :handler (partial handler opts)}]))))))
      (alter-var-root
       #'stop-watch-deps!
       (constantly
        (fn stop-watch-deps!
          []
          (swap! deps-watcher
                 (fn [watcher]
                   (when watcher
                     (hawk-stop! watcher))
                   nil))))))))

(defn load-integrant-auto-reset!
  []
  (when (try-require 'integrant-repl-autoreload.core)
    (intern 'user 'start-auto-reset (resolve 'integrant-repl-autoreload.core/start-auto-reset))
    (intern 'user 'stop-auto-reset (resolve 'integrant-repl-autoreload.core/stop-auto-reset))
    :ok))

(defn start-nrepl!
  []
  (when (try-require 'nrepl.cmdline)
    (future
      (let [set-signal-handler! (resolve 'nrepl.cmdline/set-signal-handler!)
            handle-interrupt (resolve 'nrepl.cmdline/handle-interrupt)
            dispatch-commands (resolve 'nrepl.cmdline/dispatch-commands)
            clean-up-and-exit (resolve 'nrepl.cmdline/clean-up-and-exit)]
        (println "Starting nrepl")
        (try
          ;; Be aware of the first two implicit arguments &form and &env
          (set-signal-handler! nil nil "INT" handle-interrupt)
          (dispatch-commands {:middleware ['cider.nrepl/cider-middleware
                                           'refactor-nrepl.middleware/wrap-refactor]})
          (catch clojure.lang.ExceptionInfo ex
            (let [{:keys [::kind ::status]} (ex-data ex)]
              (when (= kind ::exit)
                (clean-up-and-exit status))
              (throw ex)))
          (catch Exception e
            (println "Something went wrong!")
            (prn e)))))
    :ok))

(defn start-shadow-cljs!
  [builds]
  (when (and (try-require 'shadow.cljs.devtools.api)
             (try-require 'shadow.cljs.devtools.server)
             (try-require 'shadow.cljs.silence-default-loggers))
    (let [shadow-start-server! (resolve 'shadow.cljs.devtools.server/start!)
          shadow-from-cli (resolve 'shadow.cljs.devtools.server/from-cli)]
      (println "Starting shadow-cljs")
      (shadow-start-server!)
      (shadow-from-cli :watch (mapv keyword builds) {}))))

(defn load-clj!
  [aliases]
  (load-debugging! false)
  (install-better-deps-watcher!)
  (start-watch-deps! {:aliases aliases})
  (load-integrant-auto-reset!)
  (start-nrepl!)
  :ok)

(defn load-cljs!
  [aliases builds]
  (load-debugging! true)
  (install-better-deps-watcher!)
  (start-watch-deps! {:aliases aliases})
  (load-integrant-auto-reset!)
  ;; May be able to infer from shadow-cljs whether we need this
  #_(start-nrepl!)
  (start-shadow-cljs! builds)
  :ok)

;; Escapee thread hunting

(defn parked-thread?
  [^Thread thread]
  (let [[method-name classname]
        (some-> thread
                .getStackTrace
                first
                ((juxt #(.getMethodName %) #(.getClassName %))))]
    (and (= "park" method-name)
         (= "jdk.internal.misc.Unsafe" classname))))

(defn normal-thread?
  [^Thread thread]
  (= java.lang.Thread (.getClass thread)))

(defn system-thread?
  [^Thread thread]
  (= "system" (-> thread .getThreadGroup .getName)))

(defn cider-thread?
  [^Thread thread]
  (or (.startsWith (.getName thread) "nREPL")
      (some #(re-find #"^(nrepl\.|refactornrepl)" (.getClassName %)) (.getStackTrace thread))))

(defn main-thread?
  [^Thread thread]
  (= "main" (.getName thread)))

(defn sleep-element?
  [^StackTraceElement element]
  (and (= "sleep" (.getMethodName element))
       (= "java.lang.Thread" (.getClassName element))))

(defn thread->simple-string
  [^Thread thread]
  (let [element (->> (.getStackTrace thread)
                     (remove sleep-element?)
                     first)]
    (format "%s %s/%s:%s"
            (.getName thread)
            (.getClassName element)
            (.getMethodName element)
            (.getLineNumber element))))

(defn interrupt-thread-by-name!
  [name]
  (->> (Thread/getAllStackTraces)
       keys
       (filter #(= name (.getName %)))
       first
       (.interrupt))
  name)

(defn list-active-threads
  []
  (->> (Thread/getAllStackTraces)
       keys
       (filter normal-thread?)
       (remove parked-thread?)
       (remove system-thread?)
       (remove main-thread?)
       (remove cider-thread?)
       (map thread->simple-string)))

nil
