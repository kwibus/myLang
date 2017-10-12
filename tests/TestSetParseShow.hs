module TestSetParseShow where

import qualified ExampleLambda as L
import MakeTerm

import Lambda

basic :: [(String, LamTerm () () Name)]
basic =
  [ ("a b", appl (var "a") (var "b"))

  , ("a b c", appl (appl (var "a") (var "b")) (var "c"))

  , ("a (b c)", appl (var "a") (appl (var "b") (var "c")))

  , ("\\x.m n", lambda "x" (appl (var "m") (var "n")))

  , ("(\\x.m) n", appl ( lambda "x" (var "m")) (var "n"))

  , ("\\y x.x", lambda "y" (lambda "x" (var "x")))

  , ("(\\a.a) (\\b.b) \\c.c", appl (appl (L.id "a") (L.id "b")) (L.id "c"))
  ]

letSet :: [(String, LamTerm () () Name)]
letSet =
  [ ("let a = a;" ++
   "\nin a", mkLet [("a", var "a")] (var "a"))

  , ("let b = 1.0;" ++
   "\n    a = b;" ++
   "\nin a", mkLet [("b", double 1.0), ("a", var "b")] (var "a"))

  , ("let id a = a;" ++
   "\nin id", mkLet [("id", lambda "a" $ var "a")] (var "id"))

  , ("let const a b = a;" ++
   "\nin const", mkLet [("const", lambda "a" $ lambda  "b" $ var "a" )] (var "const"))

  , ("let b = 1.0;" ++
   "\nin + (+)", mkLet [("b", double 1.0)] $ lambda "#" $ appl (appl plus (var "#")) plus)
  , ("\\a.let b = False;"++
     "\n   in 1.0",lambda "a" $ mkLet [("b",false)] $ double 1)
  ]

math :: [(String, LamTerm () () Name)]
math =
  [ ( "+", plus)
  , ( "1.0 +", appl plus (double 1))
  , ( "+ 1.0", lambda "#" (appl (appl plus ( var "#")) (double 1)))
  , ( "1.0 + 2.0", appl (appl plus (double 1)) (double 2))
  , ( "1.0 * 2.0 + 3.0",
          appl (appl plus
                     (appl (appl multiply (double 1.0)) (double 2.0)))
                     (double 3.0))

  , ( "1.0 * (2.0 + 3.0)",
          appl (appl multiply
                     (double 1.0))
                     (appl (appl plus (double 2.0)) (double 3.0)))

  , ( "1.0 + 2.0 * 3.0",
         appl (appl plus
               (double 1))
               (appl (appl multiply
                          (double 2 ))
                          (double 3)))

  , ( "(1.0 + 2.0) * 3.0",
         appl (appl multiply
               (appl (appl plus
                          (double 1 ))
                          (double 2)))
               (double 3))
  , ( "1.0 + 2.0 + 3.0",
      appl (appl plus
             (appl (appl plus (double 1.0)) (double 2.0)))
             (double 3.0))

  , ( "1.0 + (2.0 + 3.0)",
      appl (appl plus
                    (double 1.0))
                     (appl (appl plus (double 2.0)) (double 3.0)))
  , ( "1.0 * 2.0 + 3.0 * 4.0",
      appl (appl plus
                (appl (appl multiply (double 1.0)) (double 2.0)))
                (appl (appl multiply (double 3.0)) (double 4.0)))

  , ( "1.0 + 2.0 * 3.0 + 4.0",
      appl (appl plus
                 (appl (appl plus
                             (double 1.0))
                             (appl (appl multiply
                                         (double 2.0))
                                         (double 3.0))))
                 (double 4.0))

  , ( "(1.0 + 2.0 + 3.0) * 4.0",
      appl (appl multiply
                  (appl (appl plus
                              (appl (appl plus (double 1.0)) (double 2.0)))
                              (double 3.0)))
                 (double 4.0))
  ]


advanced :: [(String, LamTerm () () Name)]
advanced =
  [ ("(\\a.a) (1.0 2.0)", appl (L.id "a") (appl (double 1.0) (double 2.0)))

  , ("1.0 2.0 +", appl plus (appl (double 1.0) (double 2.0)))

  , ("(1.0 + 2.0) 3.0", appl (appl ( appl plus (double 1.0)) (double 2.0))
                             (double 3.0))
  , ("(1.0 + 2.0) (3.0 4.0)", appl (appl (appl plus (double 1.0 )) (double 2.0))
                                   (appl (double 3.0) (double 4.0)))

  , ("(1.0 + 2.0) 3.0 4.0", appl (appl (appl (appl plus (double 1.0 )) (double 2.0))
                                       (double 3.0))
                                 (double 4.0))

  , ("\\a.1.0 +", lambda "a" (appl plus (double 1.0)))

  , ( "* \\a.a", lambda "#" (appl ( appl multiply ( var "#" )) (L.id "a")))

  , ( "(\\a.a) (*)", appl (L.id "a") multiply)

  , ("(\\a.a) *", appl multiply (L.id "a"))

  , ("\\a.a (+) (*)", lambda "a" (appl (appl (var "a") plus) multiply))

  , ("(\\s.1.0) (*) +", appl plus (appl (lambda "s" (double 1.0)) multiply))

  , ("(\\s.s) 1.0 +", appl plus (appl (lambda "s" (var "s")) (double 1.0)))

  , ("1.0 + (*)", appl (appl plus (double 1.0) ) multiply)

  , ("+ (*)", lambda "#" (appl (appl plus (var "#")) multiply))

  , ("+ (+)", lambda "#" (appl (appl plus (var "#")) plus))

  , ("(+) +", appl plus plus)

  , ("(1.0 +) +", appl plus (appl plus (double 1.0)))

  , ("1.0 (1.0 +)", appl (double 1.0) (appl plus (double 1.0)))

  , ("(\\a b.a + 1.0 * b) 2.0 3.0", appl (appl (lambda "a" (lambda "b"(appl (appl plus (var "a"))
                                            (appl (appl multiply (double 1.0)) (var "b") ))))
                                (double 2.0))(double 3.0))

  , ("(\\a.a + 1.0 *) 2.0 3.0", appl (appl (lambda "a" (lambda "##"(appl (appl plus (var "a"))
                                            (appl (appl multiply (double 1.0)) (var "##") ))))
                                (double 2.0))(double 3.0))


  , ("(+ 1.0 +) 2.0 3.0", appl (appl (lambda "#" ( appl plus
                                           (appl (appl plus (var "#"))(double 1.0))))
                                    (double 2.0))
                            (double 3.0))

  , ("(+ 1.0 *) 2.0 3.0", appl (appl (lambda "#" (lambda "##"(appl (appl plus (var "#"))
                                            (appl (appl multiply (double 1.0)) (var "##") ))))
                                (double 2.0))(double 3.0))

  , ("1.0 \\t.1.0 1.0", appl (double 1.0) (lambda "t" (appl (double 1.0) (double 1.0))))
 ]

