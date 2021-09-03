# ASAP-Unicode
ANNEXI-STRAYLINE AURA Public (ASAP) Repository - Unicode utilities

This subsystem is a member of the larger [ASAP Repository](https://github.com/annexi-strayline/ASAP)

This subsystem provides some common unicode facilities, and associated auto-generation programs.

Of particular note is the UTF8_Stream_Decoder package. This package allows for stream decoding of UTF8, in contrast to the Ada standard library facilities that require complete strings. Furthermore, the UTF8 stream decoder is written in SPARK and has been formally verified. This stream parser is designed to process external (untrusted) input data.

Another facility is the simple case folding, which is useful for properly parsing Ada program text.

