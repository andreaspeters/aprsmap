# APRSMap - Cross Platform APRS Client.

[![](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=ZDB5ZSNJNK9XQ)

## Features

- Receive APRS Messages via Radio through FlexPacket.
- Receive APRS Messages via IGate
- Support for Local (Offline) OSM Tiles

## Download

- You will find the current and all last releases for Windows and Linux [here](https://github.com/andreaspeters/aprsmap/releases)
- The newest builds created by the master branch, you will find [here](https://github.com/andreaspeters/aprsmap/actions). Open
  the last successfull build and scroll down to the artifacts.

## Requirements

- [libqt6pas.so/dll](https://github.com/davidbannon/libqt6pas/releases)

## How to compile

- Install Lazarus 3.6
- Install TRichMemo, LazMapViewer (it has to be [this version](https://github.com/wp-xyz/LazMapViewer.git))

## How to use:

### Set IGate Filter

It is possible to configure a IGate filter. To do that, open 'Settings' and 
change the value of Filter. The default one is:

```
r/<LAT>/<LON>/200
```

`r` Stands for the "Range" Filter. `<LAT>` and `<LON>` will be replaced with 
the values of Latitude and Longitude. `200` is the range in km arround
your position.

Take a look into the official [APRS-IS manual](https://www.aprs-is.net/javAPRSFilter.aspx)
to find more examples for filters.


# Screenshots

![APRSMap](vx_images/image.png)


