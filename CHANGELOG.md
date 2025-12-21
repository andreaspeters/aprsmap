# Changelog

- ADD: Wayland support
- ADD: Dock lastseen window at the main window if they are close
       to eachother.
- ADD: [gps] Add support for gpsd to get current position.
- ADD: posibility to disable/enable IGate and Dump1090.
- ADD: Start support for non position Messages.
- ADD: Start support for Compressed Data.
- FIX: WX Charts without scroolbox.

## v0.6.1

- FIX: wrong callsings from hotspots aprs packets.
- FIX: cleanup speed if it's empty

## v0.6.0

- ADD: [last] Window of the last seen APRS objects.
- ADD: [raw] Show RAW Messages from a APRS object.
- ADD: Altitude chart for tracking objects.
- CHANGE: PoI data move into seperate tab sheets.
- FIX: ModeS airplace visibility.
- ADD: Support for RS41 Ballon probes.
- FIX: Wrong APRS overlay images.
- ADD: More wather charts.
- FIX: Delete old PoIs

## v0.5.0

- ADD: ModeS (Airplane Tracking) support via dump1090.
- CHANGE: Improve PoI update.
- ADD: Tracking for APRS Objects (has to be enabled per Object).
- ADD: Splitter to resize Map and PoI details.                          *
- ADD: support for overlay image.
- ADD: Simple PoI filter.
- ADD: Configure own APRS symbol.

## v0.4.0

- ADD: Support for local OSM Tiles
- FIX: Show APRS Messages piped from FlexPacket
- FIX: PoI Update

## v0.3.0

- ADD: Support for Status reports.
- FIX: Create PoI from APRS data received over FlexPacket.
- FIX: Error during cleanup old APRS data.
- ADD: Now we can configure how old the APRS Data could be before we clean them up.
- CHANGE: Folder Icon

## v0.2.0

- ADD: Show locator in the Statusbar beside the GPS Position.
- FIX: Reset WX Data 
- FIX: missing APRS data if using Radio and not IGate.
- ADD: Set IGate filters under Settings.
- ADD: Multi Map Provider support.


