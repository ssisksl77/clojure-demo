(ns core-async-demo.core
  (:require [clojure.core.async :as a]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

;; use (chan) to make an unbuffered channel:
(chan)

;; create a channel with a fixed buffer""
(chan 1)


;; 일단 채널이라는 것이 있고 그곳에 값을 넣고 빼내는 것이라 할 수 있다.
(def my-channel (a/chan))
;; 그런데 이상태로 넣으면 멈춘다. 버퍼가 없기 때문에 더이상 테스트를 할 수 없다. 버퍼를 추가하자.
(def my-chan (a/chan 10))
;; 이제 이 채널에 값을 넣고 뺄 것이다. 방식은 두 가지가 있을 것이다.
;; 동기적으로 가져오던가 비동기적으로 가져오던가.
;; 동기적 >!, <!  비동기적 >!!, <!! 느낌표는 클로저에서 사이드이펙트를 일으키는 경우에 많이 쓰인다.
;; 동기적으로 채널을 넣고 뺄 때, 프로그램이 더 위험할 수 있어서(?) 느낌표가 더 있다고 생각해보자.
;; 책에서 !!는 대기호출이라고 한다.

;; 내 채널에 나는 값을 넣어볼 것이다.
(a/>!! my-chan :hi-my-channel-1)
(a/<!! my-chan)
(a/>!! my-chan :hi-my-channel-2)
(a/>!! my-chan :hi-my-channel-3)
(a/>!! my-chan :hi-my-channel-4)

(a/close! my-chan)

;; 클로즈를 했어도 버퍼에 남아있는 채널은 살아있다.
(a/<!! my-chan)
;; nil은 특별한 값이다. nil로 채널을 사용할 수 없음을 알 수 있다. 그러므로 nil을 넣어서는 안되고 넣을 수도 없다.


;; 비동기 입력과 비동기 출력을 사용한 것.
(let [my-chan (a/chan)]
  (a/go (a/>! my-chan :hi-my-chan-1))
  (a/go (println "Oh this is " (a/<! my-chan))))
;; 비동기로 출력한다는 말은 뒷단(백그라운드)에서 돈다는 말임.
(def my-chan (a/chan 10))
(a/go-loop []
  (println "Oh this is " (a/<! my-chan))
  (recur))
;; 위에처럼 무한루프를 돌릴 수 있다. 이 go-loop는 백그라운드에서 my-chan에 값이 들어오기를 기다린다.
;; 이 상태에서 값은 넣으면 화면에 값이 출력될 것이다.
(a/>!! my-chan :hi-this-is-young)
(a/>!! my-chan :hi-this-is-nam)
(a/>!! my-chan :hi-this-is-kim)

;; go블럭은 특별한 스레드풀에서 관리된다.
;; core.async는 여러개의 채널을 한꺼번에 관리할 수 있고, 가장 먼저 도착한 녀석의 값을 가져올 수 있따.
;; alts! 함수를 이용하면 된다.
(def chan-1 (a/chan 10))
(def chan-2 (a/chan 10))
(def chan-3 (a/chan 10))

;; alts!에 3개의 채널을 넣었고 그 리턴값은 벡터이다.
;; 벡터의 첫번째 값과 두번째 값을 destructuring 하였다.
(a/go-loop []
  (let [[v ch] (a/alts! [chan-1 chan-2 chan-3])]
    (println "값은 " v ", 어느채널에서 왔니? " ch)
    (recur)))

(a/>!! chan-3 "HI this is CHANNEL THREE")
(a/>!! chan-2 "안녕 나는 채널 2라고 해")
(a/>!! chan-1 "안녕 나는 채널 1이야")

;;;;;;;;;;;;;;;;;;;;;;
