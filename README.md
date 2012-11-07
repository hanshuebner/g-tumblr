# g-tumbler - Automatically upload images from SD-Card to tumblr

Some cameras have functionality to queue images for printing from the
built-in UI.  Such print jobs are stored as DPOF files on the camera's
mass storage medium.  This program reads DPOF files and uploads images
queued to tumblr.

In order to poll for the insertion of an SD-Card, OSX specific APIs
are used.  At this point, this program thus is OSX-specific and, due
to its usage of CCL specific APIs, not portable to other Lisp
implementations.

## Links

 * http://stackoverflow.com/questions/12409458/detect-when-a-volume-is-mounted-on-os-x
 * [DPOF summary](http://panasonic.jp/dc/dpof_110/white_e.htm)

