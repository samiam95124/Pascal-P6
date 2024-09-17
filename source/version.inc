{*******************************************************************************

                               Version numbers

The Pascal-P6 uses a common version number for all of its components. The number
is:

    major.minor[.x]

Version numbers track releases, meaning that there exists an archive with the
full repo in it under that version. Releases have extensive testing 
requirements.

The repo normally has the "experimental" flag set, meaning that any version
created from source is considered experimental. It is only set off when a
version is generated, then set back on after that.

*******************************************************************************}

   majorver   = 0; { major version number }
   minorver   = 4; { minor version number }
   experiment = true; { is version experimental? }