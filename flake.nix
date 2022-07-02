{
  description = "A dev-shell for experimenting with DAP support in Helix with ErlangLS";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    erlang_ls = {
      url = "github:the-mikedavis/erlang_ls/nopr-flake-dap-fixes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    helix = {
      url = "github:helix-editor/helix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, erlang_ls, helix, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        erlang-ls = erlang_ls.packages.${system}.default;
        hx = helix.packages.${system}.default;
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.erlangR25
            (pkgs.rebar3.overrideAttrs (_: { buildInputs = [ pkgs.erlangR25 ]; }))
            pkgs.curl
            erlang-ls
            hx
          ];
        };
      }
    );
}
