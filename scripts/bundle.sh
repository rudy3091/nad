#!/usr/bin/env bash
# Build nad and wrap it in build/nad.app.
#
# Why a bundle: macOS attributes the Accessibility permission to whatever
# process is "responsible" for the call. Run straight from a terminal, that is
# the terminal app, so the grant follows the terminal instead of nad. Inside a
# bundle nad is responsible for itself.
#
# Why signed: TCC remembers an unsigned binary by path, so every rebuild loses
# the grant. Signed with a stable identity it is remembered by that identity and
# survives rebuilds — including the ones `nad --recompile` will do.
#
# One-time setup for a stable identity (Keychain Access ›
# Certificate Assistant › Create a Certificate: name "nad-dev", type
# "Code Signing", self-signed), then:
#
#   NAD_SIGN_IDENTITY=nad-dev scripts/bundle.sh
#
# Without it the bundle is signed ad-hoc and the permission has to be
# re-granted after each rebuild.
set -euo pipefail

cd "$(dirname "$0")/.."

IDENTITY="${NAD_SIGN_IDENTITY:--}"
APP="build/nad.app"

cabal build exe:nad
BIN="$(cabal list-bin exe:nad)"

rm -rf "$APP"
mkdir -p "$APP/Contents/MacOS"
cp "$BIN" "$APP/Contents/MacOS/nad"

cat > "$APP/Contents/Info.plist" <<'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleExecutable</key><string>nad</string>
  <key>CFBundleIdentifier</key><string>dev.rudy3091.nad</string>
  <key>CFBundleName</key><string>nad</string>
  <key>CFBundlePackageType</key><string>APPL</string>
  <key>CFBundleShortVersionString</key><string>0.1.0</string>
  <!-- No Dock icon: nad is an agent, not an app the user switches to. -->
  <key>LSUIElement</key><true/>
</dict>
</plist>
PLIST

codesign --force --deep --sign "$IDENTITY" "$APP"

# The bundle is only the executable. ~/.nad/nad.hs imports the *library*, so
# without reinstalling it a source change never reaches the config binary —
# `nad restart` just re-runs the same stale one and nothing appears to change.
cabal install --lib lib:nad --force-reinstalls

# Each reinstall registers another copy, and two make `import Nad` ambiguous.
# The environment file names the one to keep, and the db holding it.
ENV=$(ls -d "$HOME"/.ghc/*-"$(ghc --numeric-version)"/environments/default)
DB=$(awk '$1 == "package-db" { print $2 }' "$ENV")
KEEP=$(awk '$1 == "package-id" { printf "%s ", $2 }' "$ENV")
for id in $(ghc-pkg field --package-db="$DB" nad id --simple-output); do
  case " $KEEP " in
    *" $id "*) continue ;;
  esac
  ghc-pkg unregister --package-db="$DB" --force --unit-id "$id"
done

# Rebuild the user's config against what was just installed. Unconditional:
# nad skips the rebuild when nad.hs is older than its binary, which it is.
if [ -f "$HOME/.nad/nad.hs" ]; then
  "$APP/Contents/MacOS/nad" --recompile
fi

echo "built $APP (identity: $IDENTITY)"
echo "run: $APP/Contents/MacOS/nad doctor"
if [ "$IDENTITY" = "-" ]; then
  echo "note: ad-hoc signed — Accessibility must be re-granted after each rebuild."
  echo "      see the header of this script for the stable-identity setup."
fi
