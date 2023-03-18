(ns app.rpg

  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.api :as hf]
            [clojure.string :as str]
            [hyperfiddle.electric-svg :as svg]))

#?(:clj (def !users (atom {})))

(e/def users (e/server (e/watch !users)))

(e/def session-id (e/server (get-in hf/*http-request* [:headers "sec-websocket-key"])))

(e/defn Debugger [x]
  (dom/div
   (dom/style {:background "white"
               :position "fixed"
               :top "0"
               :left "0"
               :z-index "2"})
   (dom/text (str x))))

(def rooms
  {:outside [[:g :g :g :g :g :g :g :g :w :w :w :g :g :g :g :g]
             [:g :r :r :r :g :g :g :w :w :w :g :g :g :g :g :g]
             [:g :h :h :h :g :g :g :w :w :w :g :g :g :g :g :g]
             [:g :h :h :h :g :g :g :w :w :g :g :r :r :r :g :g]
             [:g :g :g :g :g :w :w :w :w :g :g :h :h :h :g :g]
             [:g :g :g :g :g :w :w :w :w :g :g :h :h :h :g :g]
             [:g :g :g :w :w :w :w :w :w :g :g :g :g :g :g :g]
             [:g :g :g :w :w :w :w :w :g :g :g :g :g :g :g :g]
             [:g :g :g :w :w :w :w :g :g :g :g :g :g :g :g :g]
             [:g :g :g :w :w :w :g :g :g :g :g :g :g :g :g :g]
             [:g :g :w :w :w :w :g :g :g :g :g :g :g :g :g :g]
             [:g :g :w :w :w :w :g :g :g :g :w :w :g :g :g :g]
             [:w :w :w :w :g :g :g :g :g :w :w :w :g :g :g :g]
             [:w :w :w :w :g :g :g :g :g :w :w :w :w :g :g :g]
             [:w :w :w :g :g :g :g :g :g :w :w :w :w :w :g :g]
             [:w :g :g :g :g :g :g :g :g :w :w :w :w :w :g :g]]
   :house1  [[:f :f :o :f :f :f :f :o]
             [:f :f :f :f :f :o :f :f]
             [:f :o :o :f :o :o :o :f]
             [:o :o :o :f :o :f :f :f]
             [:o :f :o :o :o :f :o :o]
             [:f :f :f :f :f :f :o :f]
             [:f :o :o :o :o :o :o :f]
             [:f :o :f :f :f :f :f :f]
             [:f :f :f :f :f :f :o :o]]
   :house2  [[:f :f :f :f :f :f :f]
             [:f :f :o :o :o :o :f]
             [:f :f :o :o :o :o :f]
             [:f :f :f :f :f :f :f]
             [:f :f :f :f :f :f :f]]
   :tunnel1 [[:u :u :w :w :w :u :u :u :u :u :w :w]
             [:u :u :u :u :u :u :w :w :u :u :u :u]
             [:w :w :w :u :u :u :w :w :w :u :u :u]]
   :cave1 [[:w :w :w :w :w :w :w :w :w]
           [:w :w :w :w :w :w :w :w :w]
           [:w :w :w :w :w :w :w :w :w]
           [:w :w :w :u :u :u :w :w :w]
           [:w :w :w :u :u :u :w :w :w]
           [:w :w :w :u :u :u :w :w :w]
           [:w :w :w :w :w :w :w :w :w]
           [:w :w :w :w :w :w :w :w :w]
           [:w :w :w :w :w :w :w :w :w]]})

(def doors
  {:outside [{:position [12 5]
              :background "url(door.png)"
              :destination {:room :house1 :position [4 8]}}

             {:position [2 3]
              :background "url(door.png)"
              :destination {:room :house2 :position [4 4]}}]

   :house1  [{:position [4 8]
              :background "url(door.png)"
              :destination {:room :outside :position [12 5]}}
             {:position [3 3]
              :background "url(escalator-down.png)"
              :destination {:room :cave1 :position [4 4]}}
             {:position [0 0]
              :background "url(escalator-down.png)"
              :destination {:room :tunnel1 :position [11 2]}}]

   :house2  [{:position [4 4]
              :background "url(door.png)"
              :destination {:room :outside :position [2 3]}}
             {:position [0 0]
              :background "url(escalator-down.png)"
              :destination {:room :tunnel1 :position [0 1]}}]
   
   :tunnel1 [{:position [11 2]
              :background "url(escalator-down.png)"
              :destination {:room :house1 :position [0 0]}}
             {:position [0 1]
              :background "url(escalator-down.png)"
              :destination {:room :house2 :position [0 0]}}]

   :cave1 [{:position [4 4]
            :background "url(escalator-down.png)"
            :destination {:room :house1 :position [3 3]}}]})

(defn room-dimensions [room]
  (let [room (get rooms room)]
    [(count (first room)) (count room)]))

(defn get-tile [x y room]
  (-> (get rooms room)
      (nth y)
      (nth x)))

(defn tile-terrain-walkable? [x y room]
  (contains? #{:g :f :u} (get-tile x y room)))

(defn tile-in-bounds? [x y room]
  (let [[w h] (room-dimensions room)]
    (and (>= x 0) (>= y 0) (< x w) (< y h))))

(defn tile-walkable? [x y room]
  (and (tile-in-bounds? x y room)
       (tile-terrain-walkable? x y room)))

(defn random-free-tile [room]
  (let [[w h] (room-dimensions room)]
    (loop [x (rand-int w)
           y (rand-int h)]
      (if (tile-walkable? x y room)
        [x y]
        (recur (rand-int w) (rand-int h))))))

(e/defn Sprites []
  (dom/div
   (e/for [door (get doors (:room (get users session-id)))]
     (let [[x y] (:position door)]
       (dom/div (dom/style {:position "absolute"
                            :width "var(--tile-size)"
                            :height "var(--tile-size)"
                            :background (:background door)
                            :background-size "cover"
                            :image-rendering "pixelated"
                            :left (str "calc(" x " * var(--tile-size))")
                            :top (str "calc(" y " * var(--tile-size))")
                            :font-size "20px"
                            :text-align "center"
                            :line-height "var(--tile-size)"
                            :transition "all 0.1s ease-in-out"}))))
   (e/for-by key [[k v] users]
             (when (= (:room v) (:room (get users session-id)))
               (let [[x y] (:position v)]
                 (dom/div
                  (dom/style {:position "absolute"
                              :width "var(--tile-size)"
                              :height "var(--tile-size)"
                              :background "url(player.png)"
                              :background-size "cover"
                              :image-rendering "pixelated"
                              :left (str "calc(" x " * var(--tile-size))")
                              :top (str "calc(" y " * var(--tile-size))")
                              :font-size "calc(var(--tile-size) / 3)"
                              :filter (str "hue-rotate(" (mod (hash k) 360) "deg)")
                              :text-align "center"
                              :line-height "var(--tile-size)"
                              :transition "all 0.1s ease-in-out"}))
                 (when
                  (not-empty (:message v))
                   (dom/div
                  ; speech bubble
                    (dom/style {:position "absolute"
                                :width "calc(var(--tile-size) * 2)"
                                :height "calc(var(--tile-size) / 2)"
                                :white-space "nowrap"
                                :left (str "calc(" x " * var(--tile-size) - var(--tile-size) / 2)")
                                :top (str "calc(" y " * var(--tile-size) - var(--tile-size) / 2 - 10px)")
                                :text-transform "uppercase"
                                :overflow "hidden"
                                :background "white"
                                :color "black"
                                :text-align "center"
                                :z-index "1"
                                :padding "10px"
                                :font-size "calc(var(--tile-size) / 3)"
                                :line-height "50%"
                                :border "5px groove #ccc"
                                :border-bottom-left-radius "0"
                                :transition "all 0.1s ease-in-out"})
                    (dom/text (:message v)))))))))

(e/defn Message-box []
  (dom/input (dom/style {:position "absolute"
                         :width "calc(var(--tile-size) * 6)"
                         :height "var(--tile-size)"
                         :left "8px"
                         :bottom "8px"
                         :background "white"
                         :font-family "DotGothic16"
                         :text-transform "uppercase"
                         :border "2px solid #ccc"
                         :padding "10px"
                         :z-index "5"
                         :font-size "calc(var(--tile-size) / 2)"
                         :line-height "calc(100% - 20px)"
                         :outline "none"
                         :transition "all 0.1s ease-in-out"})
             (dom/props {:maxlength "9"
                         :id "message-box"
                         :placeholder "type something !"
                         :value (get-in users [session-id :message])})
             (dom/on "input" (e/fn [e]
                               (let [target (.-target e)
                                     value (.-value target)]
                                 (e/server
                                  (swap! !users assoc-in [session-id :message] value)))))))

(e/defn Map []
  (dom/div
   (dom/style {:position "relative"
               :background "black"})
   (e/for [row (get rooms (:room (get users session-id)))]
     (dom/div
      (dom/style {:display "flex"})
      (e/for [tile row]
        (dom/div
         (dom/style {:width "var(--tile-size)"
                     :height "var(--tile-size)"
                     :image-rendering "pixelated"
                     :background (case tile
                                   :g "url(grass.png)"
                                   :w "url(water.gif)"
                                   :h "url(wall.png)"
                                   :r "url(roof.png)"
                                   :f "url(floor.png)"
                                   :u "url(underground.png)"
                                   :o "url(ottoman.png)")
                     :background-size (case tile
                                        :w "50%"
                                        :h "50%"
                                        :f "50%"
                                        :r "50%"
                                        "cover")})))))
   (Sprites.)))




(e/defn App []
  (dom/style {:background "black"
              :font-family "'DotGothic16', sans-serif"
              :display "grid"
              :place-items "center"
              :margin "0"
              :overflow "hidden"}) 
  (dom/div
   (dom/style {:position "relative"
               :width "100vw"
               :height "100vh"
               :background "black"
               :display "grid"
               :place-items "center"})
   (dom/div (dom/style {:position "relative"
                        :width "calc(var(--tile-size) * 16)"
                        :height "calc(var(--tile-size) * 16)"
                        :display "grid"
                        :place-items "center"})
            (Map.)
            (Message-box.)))
  ; Event handler
  (dom/on "keydown" (e/fn [e]
                      (let [key (.-key e)]
                        (if (contains? #{"ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown"} key)
                          (do
                            (.blur (.getElementById js/document "message-box"))
                            (e/server
                             (let [[x y] (:position (get @!users session-id))
                                   next-x (case key
                                            "ArrowLeft" (dec x)
                                            "ArrowRight" (inc x)
                                            x)
                                   next-y (case key
                                            "ArrowUp" (dec y)
                                            "ArrowDown" (inc y)
                                            y)]
                               (println "next-x" next-x "next-y" next-y)
                               (let [door (first (filter #(= [next-x next-y] (:position %))
                                                         (get doors (:room (get @!users session-id)))))]
                                 (if door
                                   (println "door" (swap! !users #(-> %
                                                                      (assoc-in [session-id :room] (get-in door [:destination :room]))
                                                                      (assoc-in [session-id :position] (get-in door [:destination :position])))))
                                   (when
                                    (tile-walkable? next-x next-y (:room (get @!users session-id)))
                                     (println "move" (swap! !users assoc-in
                                                            [session-id :position]
                                                            [next-x next-y]))))))))
                          (.focus (.getElementById js/document "message-box"))))))

  

  ; Event handlers
  ; Detect when user joins/leaves
  (e/server
   (swap! !users assoc session-id {:position (random-free-tile :outside)
                                   :room :outside})
   (e/on-unmount #(swap! !users dissoc session-id))))




