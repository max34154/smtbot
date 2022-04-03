(defproject smtbot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [morse "0.4.3"]
                 [bidi "2.1.6"]
                 [buddy/buddy-core "1.7.1"]
                 [http-kit "2.5.3"]
                 [ring/ring-core "1.9.3"]
                 [ring "1.9.0"]
                 [hikari-cp "2.13.0"]
                 [org.clojure/tools.logging "1.1.0"]
                 [metosin/ring-http-response "0.9.2"]
                 [io.forward/yaml "1.0.10"]
                 [com.taoensso/timbre "5.1.2"]
                 [ring-middleware-format "0.7.4" :exclusions [ring]]
                 [ring/ring-json "0.5.1"]
                 [org.clojure/java.jdbc "0.7.12"]
                 [com.h2database/h2 "1.4.200"]
                 [org.postgresql/postgresql "42.2.24"]
                 [de.ubercode.clostache/clostache "1.4.0"]
                 [clj-time "0.15.2"]
                 [http-kit.fake "0.2.1"]]
  :main ^:skip-aot smtbot.core
  :target-path "target/%s"
  :test-paths ["test"]
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
