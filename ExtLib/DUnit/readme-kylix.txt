The separate releases for Kylix are no longer provided. 
Up-to-date Kylix code is inluded in the main source.

The following instructions relate to the last separate Kylix build (7.0.3)
To install DUnit for Kylix, unzip and and untar into a destination directory
of your choice and include it in your unit search path.

Currently, standalone test runner is not available in Kylix.

KylixGUITestRunner has been replaced by QGUITestRunner. KylixGUITEstRunner is
no longer provided

The documentation is beeing revised to cover Kylix specific issues. In DUnit
for Kylix changes to the programmer interface are minimal. The only
difference is that it's build on CLX rather than VCL, so the GUI test runner
class name has changed to from TGUITestRunner to TQGUITestRunner and the unit
name has changed from GUITestRunner to QGUITestRunner.

Follow DUnit site on SourceForge (sf.net/projects/dunit) for latest updates.
