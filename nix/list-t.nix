{ mkDerivation, base, base-prelude, foldl, HTF, mmorph
, monad-control, mtl, mtl-prelude, stdenv, transformers
, transformers-base
}:
mkDerivation {
  pname = "list-t";
  version = "1.0.4";
  sha256 = "3863844bf18a47997dce5972df30b6a38d257cbc168216be2233a40b33c15577";
  libraryHaskellDepends = [
    base foldl mmorph monad-control mtl transformers transformers-base
  ];
  testHaskellDepends = [ base-prelude HTF mmorph mtl-prelude ];
  doCheck = false;
  homepage = "https://github.com/nikita-volkov/list-t";
  description = "ListT done right";
  license = stdenv.lib.licenses.mit;
}
