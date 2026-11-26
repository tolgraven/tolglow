#!/bin/sh
# Tethered Push 3 display steps for the Naut kit (docs/qualification/naut-hardware.md).
#
#   push3-display.sh status          IORegistry read only: location, interface-0 owner, libusb
#   push3-display.sh list            Afterglow list (reads cached descriptors, claims nothing)
#   push3-display.sh inspect         Afterglow inspect (opens the device, claims nothing)
#   push3-display.sh show-chart [S]  Afterglow show-chart: claims interface 0 for S seconds (default 12)
#
# inspect and show-chart refuse while another process (Live's Push3 helper) holds
# a user client on the display interface. libusb's macOS open asks other device
# clients to close, so neither runs against a display Live still owns.
set -eu

here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../../.." && pwd)
command=${1:-status}
seconds=${2:-12}

# IORegistry reads; nothing here opens the device.
ioreg_device() { ioreg -p IOUSB -r -n "Ableton Push 3" -l -w0 -d1 2>/dev/null; }
display_owners() {
  ioreg -r -n "Ableton Push 3 Display@0" -w0 2>/dev/null \
    | sed -n 's/^.*+-o \([^<]*\) *<class AppleUSBHostInterfaceUserClient.*$/\1/p' | sed 's/ *$//'
}
libusb_path() {
  for candidate in /opt/homebrew/lib/libusb-1.0.dylib /usr/local/lib/libusb-1.0.dylib; do
    if [ -f "$candidate" ]; then echo "$candidate"; return 0; fi
  done
  return 1
}

location=$(ioreg_device | sed -n 's/.*"locationID" = \([0-9]*\).*/\1/p' | head -1)
address=$(ioreg_device | sed -n 's/.*"USB Address" = \([0-9]*\).*/\1/p' | head -1)
if [ -z "${location:-}" ] || [ -z "${address:-}" ]; then
  echo "No tethered Ableton Push 3 in the IORegistry." >&2
  exit 2
fi
bus=$((location >> 24))
owners=$(display_owners | tr '\n' ' ')

case "$command" in
  status)
    echo "push3.usb.location-id=$(printf '0x%08x' "$location")"
    echo "push3.libusb.bus=$bus"
    echo "push3.libusb.address=$address"
    echo "push3.display.interface-0.user-clients=${owners:-none}"
    if path=$(libusb_path); then echo "libusb=$path"; else echo "libusb=missing (install it, e.g. brew install libusb, with operator approval)"; fi
    exit 0 ;;
  list|inspect|show-chart) ;;
  *) echo "Use status, list, inspect or show-chart [seconds]" >&2; exit 64 ;;
esac

if [ "$command" != list ] && [ -n "$owners" ]; then
  echo "Interface 0 is held by: $owners. Quit Live (its Push3 helper must exit) and run status again." >&2
  exit 4
fi
if ! libusb_path >/dev/null; then
  echo "libusb 1.0 is not installed; Afterglow's Push 3 driver cannot run." >&2
  exit 3
fi

cd "$root"
export JVM_OPTS="${JVM_OPTS:--Xmx3g}"
case "$command" in
  list) exec lein run -m afterglow.control.devices.push-3.qualification list ;;
  inspect) exec lein run -m afterglow.control.devices.push-3.qualification inspect "$bus" "$address" ;;
  show-chart) exec lein run -m afterglow.control.devices.push-3.qualification show-chart "$bus" "$address" "$seconds" ;;
esac
