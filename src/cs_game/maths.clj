(ns cs-game.maths)

(defmacro def-vector-op [name op]
  `(defn ~name
     ([[~'v1x ~'v1y] [~'v2x ~'v2y]]
       [(~op ~'v1x ~'v2x) (~op ~'v1y ~'v2y)])
     ([[~'v1x ~'v1y] [~'v2x ~'v2y] [~'v3x ~'v3y]]
       [(~op ~'v1x ~'v2x ~'v3x) (~op ~'v1y ~'v2y ~'v3y)])
     ([[~'v1x ~'v1y] [~'v2x ~'v2y] [~'v3x ~'v3y] [~'v4x ~'v4y]]
       [(~op ~'v1x ~'v2x ~'v3x ~'v4x) (~op ~'v1y ~'v2y ~'v3y ~'v4y)])
     ([[~'v1x ~'v1y] [~'v2x ~'v2y] [~'v3x ~'v3y] [~'v4x ~'v4y] [~'v5x ~'v5y]]
       [(~op ~'v1x ~'v2x ~'v3x ~'v4x ~'v5x) (~op ~'v1y ~'v2y ~'v3y ~'v4y ~'v5y)])))