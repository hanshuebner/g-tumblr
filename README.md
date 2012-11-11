# g-tumblr - Automatically upload images from SD-Card to tumblr

Some cameras have functionality to queue images for printing from the
built-in UI.  Such print jobs are stored as DPOF files on the camera's
mass storage medium.  This program reads DPOF files and uploads images
queued to tumblr.

In order to poll for the insertion of an SD-Card, OSX specific APIs
are used.  At this point, this program thus is OSX-specific and, due
to its usage of CCL specific APIs, not portable to other Lisp
implementations.

Most of this code contains glue to tie various open source libraries
together:

 * cl-oauth is used to implement the OAuth protocol to authenticate
   with tumblr.com.  Note that a patched version is required as POST
   requests are not properly supported by cl-oauth as of 11-Nov-2012.
   The patch is available here in my
   [github repository](https://github.com/hanshuebner/cl-oauth/commit/e63bf0d6794364bcb9830404ba1bf20df0ace288).
 * zpb-exif is used to determine the orientation of each photo so that
   it can be properly rotated before posting.
 * cl-gd is used to scale the image to a maximum width of 1200 pixels.
 * Hunchentoot is used to help with generating an OAuth token for
   tumblr.com.
 * lparallel is used for communication between the Hunchentoot
   callback handler and the main thread during OAuth token generation.
   
If you want to try this out, make sure that you've got the cl-oauth
patch as mentioned above.  Create an application on the
[tumblr Application Registration Page](http://www.tumblr.com/oauth/apps),
then invoke the configuration function:

    (g-tumblr:configure "<my-blog-name>" "<consumer-key>" "<consumer-secret>")
    
For <my-blog-name>, use the name of the tumblr blog that you want to
post to (do not include the tumblr.com domain name).  For
<consumer-key> and <consumer-secret>, use the key and secret for the
application that you have created.  The configuration function will
execute the OAuth authentication procedure at tumblr to create an
access token for your account and stores the configuration information
in the file .g-tumblr-config.lisp in your home directory.

Once you have generated the configuration, you can run

    (g-tumblr:main)
    
This function will automatically post images to tumblr which have been
queued for printing using the Camera's printing function.  I've tested
this with a Canon G10, but it might well work with other cameras that
implement the DPOF printing protocol.

## Links

 * http://stackoverflow.com/questions/12409458/detect-when-a-volume-is-mounted-on-os-x
 * [DPOF summary](http://panasonic.jp/dc/dpof_110/white_e.htm)
 * [Python one-shot OAuth access token generator](https://gist.github.com/2603387)
 * [How to encode images posted to tumblr](http://brainbackup.net/post/post-images-to-tumblr-using-phonegap-and-jsoauth.html)

