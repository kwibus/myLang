module TestSetParseShow where

import qualified ExampleLambda as L
import MakeTerm

import Lambda
import Name
import Operator

basic :: [(String, LamTerm Name () Name)]
basic =
  [ ("a b", appl (var "a") (var "b"))

  , ("a b c", appl (appl (var "a") (var "b")) (var "c"))

  , ("a (b c)", appl (var "a") (appl (var "b") (var "c")))

  , ("\\x.m n", lambda "x" (appl (var "m") (var "n")))

  , ("(\\x.m) n", appl ( lambda "x" (var "m")) (var "n"))

  , ("\\y x.x", lambda "y" (lambda "x" (var "x")))

  , ("(\\a.a) (\\b.b) \\c.c", appl (appl (L.id "a") (L.id "b")) (L.id "c"))

  ]
letSet :: [(String, LamTerm Name () Name)]
letSet =
  [ ("let a = a;" ++
   "\nin a", mkLet [("a", var "a")] (var "a"))
  , ("let b = 1.0;" ++
   "\n    a = b;" ++
   "\nin a", mkLet [("b", double 1.0), ("a", var "b")] (var "a"))
  ]

math :: [(String, LamTerm Name () Name)]
math =
  [ ( "+", val plus)
  , ( "1.0 +", appl (val plus) (double 1))
  , ( "+ 1.0", lambda "#" (appl (appl (val plus) ( var "#")) (double 1)))
  , ( "1.0 + 2.0", appl (appl (val plus) (double 1)) (double 2))
  , ( "1.0 * 2.0 + 3.0",
          appl (appl (val plus)
                     (appl (appl (val multiply) (double 1.0)) (double 2.0)))
                     (double 3.0))

  , ( "1.0 * (2.0 + 3.0)",
          appl (appl (val multiply)
                     (double 1.0))
                     (appl (appl (val plus) (double 2.0)) (double 3.0)))

  , ( "1.0 + 2.0 * 3.0",
         appl (appl (val plus)
               (double 1))
               (appl (appl (val multiply )
                          (double 2 ))
                          (double 3)))

  , ( "(1.0 + 2.0) * 3.0",
         appl (appl (val multiply)
               (appl (appl (val plus)
                          (double 1 ))
                          (double 2)))
               (double 3))
  , ( "1.0 + 2.0 + 3.0",
      appl (appl (val plus)
                     (appl (appl (val plus) (double 1.0)) (double 2.0)))
                     (double 3.0))

  , ( "1.0 + (2.0 + 3.0)",
      appl (appl (val plus)
                    (double 1.0))
                     (appl (appl (val plus) (double 2.0)) (double 3.0)))
  , ( "1.0 * 2.0 + 3.0 * 4.0",
      appl (appl (val plus)
                (appl (appl (val multiply) (double 1.0)) (double 2.0)))
                (appl (appl (val multiply) (double 3.0)) (double 4.0)))

  , ( "1.0 + 2.0 * 3.0 + 4.0",
      appl (appl (val plus)
                 (appl (appl (val plus)
                             (double 1.0))
                             (appl (appl (val multiply)
                                         (double 2.0))
                                         (double 3.0))))
                 (double 4.0))

  , ( "(1.0 + 2.0 + 3.0) * 4.0",
      appl (appl (val multiply)
                  (appl (appl (val plus)
                              (appl (appl (val plus) (double 1.0)) (double 2.0)))
                              (double 3.0)))
                 (double 4.0))
  ]


advanced :: [(String, LamTerm Name () Name)]
advanced =
  [ ("(\\a.a) (1.0 2.0)", appl (L.id "a") (appl (double 1.0) (double 2.0)))

  , ("1.0 2.0 +", appl (val plus) (appl (double 1.0) (double 2.0)))

  , ("(1.0 + 2.0) 3.0", appl (appl ( appl (val plus) (double 1.0)) (double 2.0))
                             (double 3.0))
  , ("(1.0 + 2.0) (3.0 4.0)", appl (appl (appl (val plus) (double 1.0 )) (double 2.0))
                                   (appl (double 3.0) (double 4.0)))

  , ("(1.0 + 2.0) 3.0 4.0", appl (appl (appl (appl (val plus) (double 1.0 )) (double 2.0))
                                       (double 3.0))
                                 (double 4.0))

  , ("\\a.1.0 +", lambda "a" (appl (val plus) (double 1.0)))

  , ( "* \\a.a", lambda "#" (appl ( appl (val multiply) ( var "#" )) (L.id "a")))

  , ( "(\\a.a) (*)", appl (L.id "a") (val multiply))

  , ("(\\a.a) *", appl (val multiply) (L.id "a"))

  , ("\\a.a (+) (*)", lambda "a" (appl (appl (var "a") (val plus)) (val multiply)))

  , ("(\\s.1.0) (*) +", appl (val plus) (appl (lambda "s" (double 1.0)) (val multiply)))

  , ("(\\s.s) 1.0 +", appl (val plus) (appl (lambda "s" (var "s")) (double 1.0)))

  , ("1.0 + (*)", appl (appl (val plus) (double 1.0) ) (val multiply))

  , ("+ (*)", lambda "#" (appl (appl (val plus) (var "#")) (val multiply)))

  , ("(1.0 +) +", appl (val plus) (appl (val plus) (double 1.0)))

  , ("1.0 (1.0 +)", appl (double 1.0) (appl (val plus) (double 1.0)))

  , ("(\\a b.a + 1.0 * b) 2.0 3.0", appl (appl (lambda "a" (lambda "b"(appl (appl (val plus) (var "a"))
                                            (appl (appl (val multiply) (double 1.0)) (var "b") ))))
                                (double 2.0))(double 3.0))

  , ("(\\a.a + 1.0 *) 2.0 3.0", appl (appl (lambda "a" (lambda "##"(appl (appl (val plus) (var "a"))
                                            (appl (appl (val multiply) (double 1.0)) (var "##") ))))
                                (double 2.0))(double 3.0))


  , ("(+ 1.0 +) 2.0 3.0", appl (appl (lambda "#" ( appl (val plus)
                                           (appl (appl (val plus) (var "#"))(double 1.0))))
                                    (double 2.0))
                            (double 3.0))

  , ("(+ 1.0 *) 2.0 3.0", appl (appl (lambda "#" (lambda "##"(appl (appl (val plus) (var "#"))
                                            (appl (appl (val multiply) (double 1.0)) (var "##") ))))
                                (double 2.0))(double 3.0))

  , ("1.0 \\t.1.0 1.0", appl (double 1.0) (lambda "t" (appl (double 1.0) (double 1.0))))
 ]

