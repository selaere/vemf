{
inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  crane = {
    url = "github:ipetkov/crane";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  rust-overlay = {
    url = "github:oxalica/rust-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
  };
};

description = "build webbed site";

outputs = { self, nixpkgs, crane, rust-overlay }: { packages = 
  nixpkgs.lib.genAttrs ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
    let
      pkgs = import nixpkgs {inherit system; overlays = [(import rust-overlay)];};
      rustbin = pkgs.rust-bin.stable.latest.default.override {
        targets = [ "wasm32-unknown-unknown" ];
      };
      cranelib = (crane.mkLib pkgs).overrideToolchain rustbin;
      filter = path: type: 
        builtins.match ".*/std\\.vemf$" path != null || cranelib.filterCargoSources path type;
      vemfwasm = cranelib.buildPackage {
        src = pkgs.lib.cleanSourceWith { src=cranelib.path ./..; inherit filter; };
        CARGO_PROFILE = "wasm";
        cargoExtraArgs = "--target wasm32-unknown-unknown --package vemfwasm";
        doCheck = false;
        buildInputs = [];
      };
      wasm-bindgen = pkgs.wasm-bindgen-cli; # <-ver: 0.2.92
      default = pkgs.stdenv.mkDerivation {
        pname = "vemfwasm";
        version = "0.0.1";
        dontUnpack = true;
        buildInputs = [pkgs.binaryen pkgs.esbuild wasm-bindgen];
        inherit vemfwasm;
        buildPhase = ''
          set -ex
          wasm-bindgen "${vemfwasm}/lib/vemfwasm.wasm" --target web --no-typescript --out-dir .
          wasm-opt -Os -o vemfwasm_bg.wasm vemfwasm_bg.wasm
          esbuild --minify vemfwasm.js > vemfwasm-min.js
          mv vemfwasm-min.js vemfwasm.js
        '';
        installPhase = ''
          mkdir -p $out
          cp vemfwasm* $out
        '';
      };
    in { inherit default; }
  );};
}
