(ns solved.encryption)

(def input "wclwfoznbmyycxvaxagjhtexdkwjqhlojykopldsxesbbnezqmixfpujbssrbfhlgubvfhpfliimvmnny")
(def expected "wmgjpnull cyjqlejgi lyhhdzbui wctlsqsbm fxeoxmsvv ovxjeirfm zadysxbhn nxkkbffpn bawobphfy")

(defn sqrt [n]
  (if (> n 0)
    (Math/sqrt n)
    0))

(defn x []
  (let [size (count input)
        float-num (sqrt size)
        below (int float-num)
        above (if (zero? (rem float-num below)) below (inc below))
        rows below
        cols above
        _ (assert (>= (* rows cols) size))
        parted (partition-all cols input)
        vec-parted (mapv vec parted)
        map-fn (fn [n] (mapv #(get % n) vec-parted))
        res-1 (mapv map-fn (range cols))
        res (apply str (interpose " " (mapv #(apply str %) res-1)))
        ]
    ;(println vec-parted)
    (println float-num rows cols)
    ;(println (map first vec-parted))
    (println res)))
