nix-shell -p "haskell.packages.ghc92.ghcWithPackages (pkgs: with pkgs; [
    cryptohash-sha256
    base16-bytestring
])"
#nix-shell -p "haskell.packages.ghc92.ghcWithPackages (pkgs: with pkgs; [ cryptohash ])"
