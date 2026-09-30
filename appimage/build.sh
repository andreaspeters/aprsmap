#!/usr/bin/env bash
set -euo pipefail

APPDIR="${APPDIR:-aprsmap}"
APP="../src/aprsmap"
LIBDIR="$APPDIR/usr/lib"

if [[ ! -f "$APP" ]]; then
    printf 'Application binary not found: %s\n' "$APP" >&2
    exit 1
fi

mkdir -p "$APPDIR/usr/bin" "$LIBDIR"
install -m 0755 "$APP" "$APPDIR/usr/bin/aprsmap"

# Allow callers to specify the exact library (useful in non-FHS/Nix builds).
qt6pas_lib="${QT6PAS_LIB:-}"
if [[ -z "$qt6pas_lib" ]] && command -v ldconfig >/dev/null 2>&1; then
    qt6pas_lib="$(ldconfig -p 2>/dev/null | awk '$1 == "libQt6Pas.so.6" { print $NF; exit }')"
fi

if [[ -z "$qt6pas_lib" || ! -f "$qt6pas_lib" ]]; then
    printf 'Cannot locate libQt6Pas.so.6. Set QT6PAS_LIB to its full path.\n' >&2
    exit 1
fi

# Dereference any development symlink and install it under the SONAME the
# executable requests. Set a relative RUNPATH so AppRun does not depend on
# the host loader configuration.
install -m 0644 -T "$(realpath "$qt6pas_lib")" "$LIBDIR/libQt6Pas.so.6"
if command -v patchelf >/dev/null 2>&1; then
    patchelf --set-rpath '$ORIGIN/../lib' "$APPDIR/usr/bin/aprsmap"
else
    printf 'patchelf is required to set the AppDir-relative library path.\n' >&2
    exit 1
fi

appimagetool "$APPDIR"
