문서
https://eli.thegreenplace.net/2017/clojure-concurrency-and-blocking-with-coreasync/

비디오
https://www.youtube.com/watch?v=AhxcGGeh5ho

(require '[clojure.core.async :refer [chan go go-loop timeout >! <! alt! alts!]])

<! 는 go블록 안에서 일어나야 한다.
go블록에서는, async operations들을 동기적으로 표현할 수 있다.
;; So you have the illusion of doing thins synchronously, and this eliminates the callback hell.
;; If you have a go block. it always returns a chennel. So, the result of a go block is always a channel.
;; I returned five, but what actually is going to happen is the five is inside the channel,
;; The channel has that value, but I haven't read it out.
(println (go 5))  ;; not 5 but return channel
(println (<! (go 5)))  ;; go 밖에서 <!는 안됨.
(go (println (<! (go 5))))  ;; double go block
;; You can't do a read outside of go blocks. If you want to have this illusion of being able to do things
;; asynchronously without writing callbacks, you have to do all of your operations inside the go block.
;; but <!! allowed to do reads off of channels outside of a go block, but it means that you could potentially
;; tie up a thread.
"go블록의 결과값은 항상 채널이다. 알아야 한다. 항상 go블록은 channel을 리턴한다."
"그렇기 때문에 채널 안에서 <!를 해야 하는 것이다. 밖에서 하면 안된다. 뭐 <!!를 하면 된다."
"하지만 위험은 알아서 감수하는 것!"


(let [c (chan)]
  (go
   (println "we got here")
   (<! c)
   (println "We'll never get here")))
;; now we have put(>!) and take(<!) operations. (It's primitives)
;; the semantics of CSP is that rights and reads, when you put and you take,
;; if you put sth, sbd has to take from the other side.
;; ** Otherwise, inside the go block it becomes a suspended operation.
;; If I take, and there's nothing there on the channel, then again the operation is going to suspend
;; until sbd puts sth. So this is REALLY important, and it's very much how Golay works
;; and how the original CSP model was defined. Synchronous operations

;; The channel, it's not a place, it's a conduit. I can put stuff on to it, sort of into it,
;; so that people can take things out or I can take things out.
;; The only thing that's important is that, if I try to take something out, and there'snothing there
;; I'm going to be suspended until sbd puts somthing there.

;; Type is purely dynamic. So it's not like in Go. Google's Go, you have to give the channel a type.
;; so there are untyped channels we could put anything we want, though you really should put
;; immutable values, so you can avoid the problem you have in Go with race conditions.
;; You can have data races if you put mutable values onto a channel.

;; you can also put channel. Channels are first-class, so you can put channels onto channels,
;; and there's really cool tricks you can do with that.

누군가 받기 전까지는 실행이 되지 않는다. CSP는 꼭 누군가가 받아야 한다.
구글의 GO와 다르게 뭐든 던질 수 있다. 채널조차도! GO는 타입을 지정해야 한다.

(let [c (chan)]
  (go
   (println "Me got here")
   (<! c)  ;; 채널을 기다린다.
   (println "me made progress"))
  (go  ;; 다른 go블록에서 c 채널에게 값을 보낸다.
   (>! c (new java.util.Date))))


;; the code is out-of-order
;; first go block is going to run, it's going to get suspended by the read,
;; and next go-block run, and first go-block continue (vise-versa)
;; 오더가 나오면 비포가 나오고 피포가 나오면 오더가 다온다. 서로 기다리는 것.
(let [c (chan)]
  (go
   (println "Before")
   (>! c (new java.util.Date))
   (println "After"))
  (go
   (println "Order")
   (<! c)
   (println "doesn't matter")))


(go (println "Hello")
    (<! (timeout 1000))
    (println "async")
    (<! (timeout 1000))
    (println "world!"))

timeout 채널은 값을 제공할 것이다.

Non-determistic choice
비동기 끼리도 순서가 있을 수가 있다. 이걸 먼저 하고 저걸 먼저해라 그런거
누가 일을 먼저 끝내건
alts! 메서드
(let [c (chan)  ;; 채널 만들기
      t (timeout 1000)]  ;; 타임아웃 채널 만들기
  (go
   (let [[v sc] (alts! [c t])]  ;; 채널 벡터를 받는다. 여기서 [c t] 중에 먼저 실행되는 것이 먼저 나온다.
     (println "Timeout channel closed!" (= sc t)))))
;; c 채널은 값을 반환하지 않지만, timeout channel은 값을 반환한다.
;; 그래서 c 채널이 반환하지 않아도 timeout channel이 반환하면 프린트가 실행된다.
;; alts!의 리턴값 [v cs] 에서 v는 해당 채널의 리턴값이며, cs는 리턴한 해당 채널을 말한다.
;; 그러니 (= sc t) 가 트루인 것이다. 무조건 timeout channel일 수밖에 없으니.



(let [c (chan)
      t (timeout 1000)]
  (go
   (alt!  ;; cond와 같다. 매칭하는 것! c,t 둘다 core.async의 채널임. 각 채널에 따라 매칭
    c ([v] (println "Channel responded"))  ;; 얘는 채널 자체니까 바로 받음.
    t ([v] (println "Timeout channel closed!")))))  ;; 얘는 1초 쉬는 채널이니까 일초 쉬고 일하고 끝나느 애가 된것.
위 내용은 아래와 동일하다.
(let [c (chan)  ;; 채널 만들기
      t (timeout 1000)]  ;; 타임아웃 채널 만들기
  (go
   (let [[v c'] (alts! [c t])]
    (condp = c'
     c (println "the channel had a value of closed!")
     t (println "Timeout channel closed! " (= c' t))
     "something else!"))))

alts가 더 이쁘게 보인다. syntax sugar 가 멋지다.
아래 내용은 채널 안에서 값을 저장할 수 있다.
잘 보면 함수 안으로 in이라는 채널이 하나 들어온다.
in 에서 값을 가져왔는데 last의 값이랑 다를 경우에 out으로 보낸다.
값이 같은 경우. 그냥 그 값을 가지고 loop의 처음으로 돌아간다.
그러니깐 같은 값을 계속 넣으면 무시한다는 말.
(defn distinct [in]
  (let [out (chan)]
    (go (loop [last nil]
          (let [x (<! in)]
            (when (not= x last)
              (>! out x))
            (recur x))))
    out))
테스트 해보자.
(let [dup_numbers (map #(quot % 2) (range 1 10))
      c (chan)
      res_chan (distinct c) ]
  (go (doseq [n dup_numbers]
        (>! c n)))
  (go (while true
        (println (<! res_chan)))))
중복이 사라짐. ㄷㄷ 레이지 값을 바로 실행시키기 위해 doseq를 이용.

혹은 모든 중복도 막을 수 있다. distinct를 바꿔보자.

(defn never-again [in]
  (let [out (chan)]
    (go (loop [seen #{}]
          (let [x (<! in)]
            (when-not (contains? seen x)
              (>! out x))
            (recur (conj seen x)))))
    out))

(let [dup_numbers [1 2 3 4 5 4 3 2 1]
      c (chan)
      res_chan (never-again c) ]
  (go (doseq [n dup_numbers]
        (>! c n)))
  (go (while true
        (println (<! res_chan)))))
1 2 3 4 5 만 나오게 된다. ㄷㄷㄷ..


## fan-in 30:13
# fan-in은 채널을 벡터로 받는다.
# 그리고는 싱글 채널로 머지한다. 이건 아주 유용한다.
# 엄청나게 많은 인풋이 들어온다면 그것들로 여러 일들을 할 수 있다면!
(defn fan-in [ins]
  (let [out (chan)]
    (go (while true
          (let [[x] (alts! ins)]
            (>! out x))))
    out))

# 제너레이터를 만든 현장이다.
(defn my-ints []
  (let [out (chan)]
    (go (loop [i 1]
          (>! out 1)
          (recur (inc i))))
    out))

(defn interval [msecs]
  (let [out (chan)]
    (go (while true
          (>! out (new java.util.Date))
          (<! (timeout msecs))))
    out))

