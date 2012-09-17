# fuzzbuster

Detects blurry images

## Usage

java -jar fuzzbuster.jar /home/me/blurry-pictures

java -jar fuzzbuster.jar --help
for options

Fuzzbuster actually works by detecting sharp portions of the image; an image with a shallow depth of field and an in-focus subject will not be detected as blurry. How fine-grained this effect is depends on the --sectors parameter, which is both the width and height of the grid the image is split in to for detecting sharp areas.

## Download

You can download a standalone jar from http://classifyr.com/~zak/fuzzbuster.jar

## License

Copyright © 2012 Zak Wilson

Portions copyright © Laurence Hyate

Distributed under the Mozilla Public License
