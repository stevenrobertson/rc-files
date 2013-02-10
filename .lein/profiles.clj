{:user
  {:plugins [[lein-droid "0.1.0-SNAPSHOT"]
             [lein-tarsier "0.9.3"]
             [lein-localrepo "0.4.1"]]
   :android {:sdk-path "/home/strobe/src/android-sdk-linux_86"}
   :jvm-opts ["-Xmx2500M"]
   :warn-on-reflection true
   :repl-options {
    :init (do
  (require 'clojure.reflect 'clojure.pprint)
  (defn reflect-find
    "Reflect an object and find a substring amidst its members."
    [cls key substr]
    (clojure.pprint/pprint
      (filter #(.contains (.toLowerCase (str (key %))) (.toLowerCase substr))
               (:members (clojure.reflect/reflect cls)))))
    )}}}
