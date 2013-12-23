(ns reddit-daily-programmer.core)

(defn easy-146
  [& args]
  (when args
    (let [str-args      (.split (first args) " ")
          num-of-sides  (Integer/valueOf (first str-args))
          circum-radius (Double/valueOf  (last str-args))
          side-length   (* (* (Math/sin (/ Math/PI num-of-sides)) 2) circum-radius)]
      (format "%.3f" (* num-of-sides side-length)))))
