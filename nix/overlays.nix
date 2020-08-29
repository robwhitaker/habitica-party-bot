self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: {
      habitica-hs = (hself.callPackage ../lib/habitica-hs {}).drv;
      servant-server = hself.servant-server_0_17;
      servant = hself.servant_0_17;
      req = hself.callPackage ./req_3.1.0.nix {};
    };
  };
}
