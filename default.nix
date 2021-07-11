{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.05.tar.gz") {}}:

pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
		lua53Packages.lua
		lua53Packages.busted
    ];
}
