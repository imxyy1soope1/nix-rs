{ system ? builtins.currentSystem }:

(builtins.getFlake (toString ./.)).devShells.x86_64-linux.default
