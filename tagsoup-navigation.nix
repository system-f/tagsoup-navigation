{ mkDerivation, base, checkers, lens, QuickCheck, semigroupoids
, semigroups, stdenv, tagsoup, tagsoup-selection, tasty
, tasty-hunit, tasty-quickcheck, transformers
}:
mkDerivation {
  pname = "tagsoup-navigation";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base lens semigroupoids semigroups tagsoup tagsoup-selection
    transformers
  ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://github.com/qfpl/tagsoup-navigation";
  description = "Tagsoup Navigation";
  license = stdenv.lib.licenses.bsd3;
}
