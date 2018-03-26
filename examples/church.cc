# boolean values
let tru    = \a.\b.a in
let fls    = \a.\b.b in
# boolean operators
let if     = \p.\a.\b.p a b in
let not    = \a.a fls tru in
let and    = \a.\b.a b fls in
let or     = \a.\b.a tru b in
let xor    = \a.\b.a (not b) b in
# numerical values
let zero   = \f.\s.s in
let succ   = \n.\f.\s.f (n f s) in
# numerical operators
let iszero = \n.n (\x. fls) tru in
let plus   = \a.\b.\f.\s.a f (b f s) in
let times  = \a.\b.\f.\s.a (b f) s in
let pred   = \n.\f.\s.n (\g.\h.h (g f)) (\u.s) (\u.u) in
let sub    = \a.\b. b pred a in
# pair constructor
let pair   = \x.\y.\p. p x y in
# pair operators
let fst   = \p.p (\x.\y. x) in
let snd   = \p.p (\x.\y. y) in
# Y combinator (normal order)
let Y     = \f.(\x. f (x x))(\x. f (x x)) in
# Y combinator (applicative order)
let Y'     = \f.  (\x.  f (\y. (x x) y)) (\x.  f (\y. (x x) y)) in

let fib =
  Y (\f.\x.
    if (iszero (pred(pred x)))
      (succ zero)
      (plus (f (pred x)) (f (pred (pred x))))
    )
in

fib (succ(succ(succ(succ(succ(succ(succ(succ(succ(succ zero))))))))))
