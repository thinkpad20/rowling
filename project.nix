{ mkDerivation, aeson, base, classy-prelude, containers
, context-stack, data-default, error-list, hspec
, hspec-expectations, mtl, parsec, scientific, stdenv, text
, text-render, unordered-containers
}:
mkDerivation {
  pname = "rowling";
  version = "0.1.0.1";
  src = ./.;
  buildDepends = [
    aeson base classy-prelude containers context-stack data-default
    error-list mtl parsec scientific text text-render
    unordered-containers
  ];
  testDepends = [
    aeson base classy-prelude containers context-stack data-default
    error-list hspec hspec-expectations mtl parsec scientific text
    text-render unordered-containers
  ];
  homepage = "http://github.com/thinkpad20/rowling";
  description = "A simple, easily embeddable pure-functional language with static typing and row polymorphism";
  license = stdenv.lib.licenses.mit;
}
