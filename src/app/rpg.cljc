(ns app.rpg

  (:require [hyperfiddle.electric :as e]
            [hyperfiddle.electric-dom2 :as dom]
            [hyperfiddle.api :as hf]
            [hyperfiddle.electric-ui4 :as ui]))

#?(:clj (def !users (atom {})))

(e/def users (e/server (e/watch !users)))

(e/def session-id (e/server (get-in hf/*http-request* [:headers "sec-websocket-key"])))

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

(defn room-dimensions [room-id]
  (let [room (get rooms room-id)]
    [(count (first room)) (count room)]))

(defn get-tile [x y room]
  (-> (get rooms room)
      (get y)
      (get x)))

(defn tile-walkable? [x y room]
  (contains? #{:g :f :u} (get-tile x y room)))

(defn random-free-tile [room]
  (let [[w h] (room-dimensions room)]
    (loop [x (rand-int w)
           y (rand-int h)]
      (if (tile-walkable? x y room)
        [x y]
        (recur (rand-int w) (rand-int h))))))

(e/defn User [[k v]] 
  (when (= (:room v) (:room (get users session-id)))
    (let [[x y] (:position v)]
      (dom/div
       (dom/props {:class "absolute !bg-cover center transition-all grid place-items-center"})
       (dom/style {:width "var(--tile-size)"
                   :height "var(--tile-size)"
                   :background "url(player.png)"
                   :left (str "calc(" x " * var(--tile-size))")
                   :top (str "calc(" y " * var(--tile-size))")
                   :filter (str "hue-rotate(" (mod (hash k) 360) "deg)")
                   :line-height "var(--tile-size)"}))
      (when
       (not-empty (:message v))
        ; Message bubble
        (dom/div
         (dom/props {:class "absolute overflow-hidden bg-white text-center z-20 transition-all
                              whitespace-nowrap px-2 pixel-shadow"})
         (dom/style {:font-size "calc(var(--tile-size) / 3)"
                     :left (str "calc(" x " * var(--tile-size) + var(--tile-size))")
                     :top (str "calc(" y " * var(--tile-size) - var(--tile-size) / 2)")
                     :max-width "calc(var(--tile-size) * 4)"})
         (dom/text (:message v)))))))

(e/defn Sprites []
  (dom/div
   (e/for [door (get doors (:room (get users session-id)))]
     (let [[x y] (:position door)]
       (dom/div 
        (dom/props {:class "absolute !bg-cover transition-all"}) 
        (dom/style {:background (:background door)
                    :width "var(--tile-size)"
                    :height "var(--tile-size)"
                    :left (str "calc(" x " * var(--tile-size))")
                    :top (str "calc(" y " * var(--tile-size))")}))))
   (e/for-by key [user users]
             (User. user)
             )))

(e/defn Message-box []
  (ui/input
   (get-in users [session-id :message])
   (e/fn [v] (e/server
              (swap! !users assoc-in [session-id :message] v)))
   (dom/props {:maxlength "9"
               :id "message-box"
               :placeholder "type something !"
               :value (get-in users [session-id :message])
               :class "absolute left-4 bottom-4 bg-white p-4 pixel-shadow outline-none"
               :style {:width "calc(var(--tile-size) * 6)"
                       :height "var(--tile-size)"
                       :font-size "calc(var(--tile-size) / 2)"}})))

(e/defn Map []
  (dom/div
   (dom/props {:class "relative"})
   (e/for [row (get rooms (:room (get users session-id)))]
     (dom/div
      (dom/style {:display "flex"})
      (e/for [tile row]
        (dom/div
         (dom/style {:width "var(--tile-size)"
                     :height "var(--tile-size)"
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
  (dom/props {:class "w-screen h-screen bg-black grid place-items-center overflow-hidden"}) 
  (dom/div
   (dom/style {:width "calc(var(--tile-size) * 16)"
               :height "calc(var(--tile-size) * 16)"})
   (dom/props {:class "relative grid place-items-center"})
   (Map.)
   (Message-box.))
  ; Detect when user joins/leaves
  (e/server
   (swap! !users assoc session-id {:position (random-free-tile :outside)
                                   :room :outside})
   (e/on-unmount #(swap! !users dissoc session-id)))
  ; Keyboard handler
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
                               (let [door (first (filter #(= [next-x next-y] (:position %))
                                                         (get doors (:room (get @!users session-id)))))]
                                 (if door
                                   (swap! !users #(-> %
                                                      (assoc-in [session-id :room] (get-in door [:destination :room]))
                                                      (assoc-in [session-id :position] (get-in door [:destination :position]))))
                                   (when
                                    (tile-walkable? next-x next-y (:room (get @!users session-id)))
                                     (swap! !users assoc-in
                                            [session-id :position]
                                            [next-x next-y])))))))
                          (.focus (.getElementById js/document "message-box")))))))