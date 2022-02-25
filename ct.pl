#!/usr/pkg/bin/perl
# ct.pl
# Rev B 12-Oct-2013
# added tone generator in FF-800 tone format
# still does morse, but now also includes CT (custom tone) generation to audio file.
# Modified from script written by KY8D
# Changes Copyright by Joseph Michael Haas, ke0ff
# Copyright by Gan Uesli Starling, KY8D
#
# ke0ff change notes:
# changed CLI switches:
#	--to[ne] changed minimum switch chars to accomodate the --ta[il] switch addon
#	--co[dec] changed minimum switch chars to accomodate the --ca[ll] switch addon
#	added --ram[p] switch (see new tags below)
#	added --ta[il] switch (see new tags below)
#	added --ca[ll] switch (see new tags below)
# changed cw() sub to use $rampf to allow user control of tone ramp
# added ctone2() sub to generate custom tone using new embeded tags
# added embeded tags ("x" below is user provided value):
#    *TD=x* custom tone duration (ms)
#    *T1=x* custom tone 1 (Hz)
#    *T2=x* custom tone 2 (Hz)
#    *TA=x* custom tone initial amplitude (%)
#    *TR=x* custom tone attack/decay (%/sec) -- long time version of CW ramp
#         if amplitude reaches max or min before duration of tone is complete, the amplitude
#         remains at max or min.
#    *TS1=x* custom tone 1 sweep (Hz/sec)
#    *TS2=x* custom tone 2 sweep (sweeps tone frequency).  Sets the tone  sweep rate for each
#         of the 2 tone generators.  If the target tone reaches 0Hz, the tone sweep stops (even
#         though the tone duration continues to process).  If the target tone reaches
#         ($sample_rate - 2) / 2 (NOTE: $sample_rate is the same as sample frequency)
#         the frequency stops at ($sample_rate - 2) / 2.  ($sample_rate - 2) / 2 is the highest
#         frequency possible using a discrete time system since $sample_rate / 2 aliases back to
#         0Hz.  However, it is observed that there is an audible alias in the frequencies leading
#         up to the nyquist rate.  Not sure why...
#    *RAMP=x* sets Morse tone ramp time (ms).  15 to 75 ms is the useful limit to the morse ramp value,
#         though any positive value is processed.
#    *TAIL=x* tail enable flag (0 = no head or tail, 1 = enable head and tail)
#    *CALL=x* allows user to specify callsign used in tail
# added *CT* prosign to invoke custom tone (see notes below on custom tone specs)
# added *GAP* tag to allow morse word spaces to be supressed if *GAP=0*.  Otherwise,
#	any space, tab, newline, etc... is interpreted as a space which makes CT
#	text difficult to format. *GAP=1* enables spaces.
# added *DISP* tag to enable raw D/A data dump that can be captured using re-direct to allow waveform
#	analysis (in something like LV or Excel).
# added *DOCALL* prosign.  Sends current value of CALLSIGN as CW tones.
#
# The FF-800 custom tone generator is a 2-tone system with programmable attack/decay.
# Tones are represented as follows ("<" and ">" characters are for field separation
# purposes only):
#
# <duration>,<tone1>,<tone2>,<amp>,<a/d>	; element 1
#    :
#    :
# <duration>,<tone1>,<tone2>,<amp>,<a/d>	; element N
# <0>,<0>,<0>,<0>,<0>				; end of tone
#
# The custom tone option implemented here uses a variation on the above format.
# Here, text tags are used to set duration, T1, T2, amplitude, and attack/decay.
# The tags can be in any order.  The *CT* prosign invokes the tone which writes
# the tone into the wav file.  Complex tones are formed by succesively setting the
# tone parameters, invoking the *CT* prosign, and repeating until the tone sequence
# is complete:
#
# *T1=600**T2=660**TD=500**CT**T1=720**CT*
#
# The custom tone parameters are preserved between *CT* prosigns, so only those which
# change from one element to the next need be invoked.  The above tone is a 1/2 second,
# 2-tone chord at 600/660 Hz followed by another 1/2 second chord at 660/720 Hz. *GAP=0*
# should be the first tag of the first line to remove extra audio space.
#
# Note that custom tones and morse code can be interspersed.  Just place a *GAP=0* tag
# before CT sequences, and a *GAP=1* tag after CT sequences.  Morse processing is
# always enabled regardless of the GAP status - GAP only affects the word space
# processing.
#
# A few thoughts on tone frequencies:
# The custom tone mods incorporate a restriction on the tone frequencies based on the
# nyquist rate (which is $sample_rate / 2 , the restriction imposed here is actually
# ($sample_rate - 2) / 2 ).  This is important in that swept tones will stop sweeping
# when the nyquist limit is reached.  However, there are subtle modulation effects
# that occur above $sample_rate/4 that should be considered.  These effects are minor,
# but noticable.  As a general rule, tone frequencies should be set at or below
# $sample_rate/4 and sweeps should be timed to terminate before they reach $sample_rate/4.
# The custom tone script "remembers" the last frequencies and amplitude so that they
# can be used by the next sequence, which simplifies the arrangement of these effects.
# 
# end ke0ff change notes

my $VERSION = '2013-10-12 22:49:00 Z';

# These modules you need to install.
use Audio::Wav;
use Time::HiRes qw( gettimeofday );

# These modules are standard.
use Getopt::Long;
use Config;
use warnings;
use strict;

# User default for Win32 only!
# Download "Oggenc2.8 using libVorbis v1.1.2 2005-12-08"
# from http::/www.rarewares.org/ogg.html and unzip to
# path below. Or choose some other ogg encoder and change
# path below to match. Change to emtpy string ('') if you
# do not want better quality, shorter length audio files.
# This will just be ignored on Unix/Linux/BSD/Mac OS X.
my $oggenc_path = 'C:/oggenc2.exe';

# If indeed running on Windoze, fix common user mistake. Exchange
# evil Win32-ish backslashes for righteous Perl foreslashes in path.
# Otherwise assume Unician platform and replace with universal command.
if ($Config::Config{'osname'} =~ /Win/i) { $oggenc_path =~ s/\\/\//g }
else { $oggenc_path = 'oggenc' }

# General defaults
my $txt_path    = './morse.txt';
my $codec       = 'wav';
my $lang        = 'en';
my $wav_path    = '';
my $xml_path    = '';
my $sample_rate = 8000;
my $tone        = 750;
my $max_wav_mb  = 200;
my $wpm         = 15;
my $diff_10ths  = 10; # Level of difficulty, a la Farnsworth timing.
my $wpm_incr    = 0;
my $wav_cnt     = 0;
my $koch_flag   = 0;  # Used for Koch-method output.
my $rand_flag   = 0;
my $help_flag   = 0;
my $v_flag      = 0;
my $d_flag      = 0;
my $rampf       = 2;
my $tail_op     = 1;
my %tags        = ( generator => 'gus_morse.pl', genre => "'Morse Code'" );
my $call_sign   = 'KY8D'; # For head & tail of message files. Change to your own.
# custom tone params
my $tone1       = 400;    # tone 1
my $tone2       = 440;    # tone 2
my $amp_max     = 50;     # 0-100 %
my $a_d_rate    = 0;      # attack/decay rate
my $sweep_rate1 = 0;      # tone1 sweep rate
my $sweep_rate2 = 0;      # tone2 sweep rate
my $duration    = 1000;   # ms
my $gap         = 1;      # gap (space) enable, 0 = no word gaps, else gaps for space and cntl chrs
my $n           = 0;      # custom tone sample counter - maintains phase continuity between *CT* sequences

my $xml_flag    = 0;
my @xml_buff    = (); # Storage for Morse-mail format data.

&GetOptions(
    "codec=s" => \$codec,
    "diff=f"  => \$diff_10ths,
    "help"    => \$help_flag,
    "incr=f"  => \$wpm_incr,
    "koch"    => \$koch_flag,
    "lang=s"  => \$lang,
    "max=i"   => \$max_wav_mb,
    "path=s"  => \$txt_path,
    "rand"    => \$rand_flag,
    "samp=i"  => \$sample_rate,
    "tone=i"  => \$tone,
    "verbose" => \$v_flag,
    "wpm=f"   => \$wpm,
    "xml"     => \$xml_flag,
    "ramp=f"  => \$rampf,
    "tail=i"  => \$tail_op,
    "call=s"  => \$call_sign,
);

sub quick_help {
    my $help_msg = <<END_HELP_MSG;
    
USAGE:
$0 [ options ]
  
OPTIONS:
--p[ath]    Text file path. Will output to same directory.
--w[pm]     Words per minute: 2 < wpm < ?
--to[ne]    Tone in Hz: 500 < Hz < ?
--m[ax]     Max size in MB: 1 < MB < ?
--i[ncr]    Increment of WPM, for each file 2nd thru Nth.
--s[amp]    Sample rate: 8000, 11050, 22100, etc
--d[iff]    Difficulty: 1 thru 10
--co[dec]   Audio codec: wav or ogg
--l[ang]    Language: en, eo, etc
--ran[d]    Shuffle chars of each line to randomize.
--v[erbose] Give verbose feedback.
--h[elp]    Show this message.
--ram[p]    set ramp (ms) (default = 2).
--ta[il]    if zero, omit head/tail add-ons (default = 1).
--ca[ll]    set new callsign (default = KY8D).

EXAMPLES (long):
  gus_morse.pl --path '/ram/typing_drills.txt' --wpm 10.5 --tone 750 --samp 8000 --diff 5.5 --codec ogg --rand
  gus_morse.pl --path '/ram/the_raven.txt' --wpm 18 --tone 800 --codec wav --lang eo
  
EXAMPLE (short):
  gus_morse.pl --p '/ram/message.txt' --w 18 --t 765 --s 8000 --d 1 --c ogg --r
  
DEFAULTS:
  gus_morse.pl --p './morse.txt' --w 15 --t 750 --s 11050 --d 10 --c wav --r --l en
  
END_HELP_MSG

    print "$help_msg";
}

quick_help() if $help_flag;

# Hash of known Morse code characters as reconciled between the following URLs:
# http://en.wikipedia.org/wiki/Morse_code
# http://www.netwalk.com/~fsv/CWguide.htm#Learning%20the
# http://home.alltel.net/johnshan/cw_ss_list_punc.html
# http://homepages.cwi.nl/~dik/english/codes/morse.html
# http://www.cyberbeach.net/lberta/mrsecode.html
# http://morsecode.scphillips.com/morse.html
# http://homepages.tesco.net/~a.wadsworth/MBcode.htm
# http://www.reference.com/browse/wiki/Morse_code
# http://eo.wikipedia.org/wiki/Morsa_kodo
my %morse = (

    # Punctuation
    ' ' => sub { if ($gap == 1) {stp(); stp(); gap();} },
    '.' => sub { di(); da(); di(); da(); di(); da(); stp(); },
    ',' => sub { da(); da(); di(); di(); da(); da(); stp(); },
    ':' => sub { da(); da(); da(); di(); di(); di(); stp(); },
    '?' => sub { di(); di(); da(); da(); di(); di(); stp(); },
    "'" => sub { di(); da(); da(); da(); da(); di(); stp(); },
    '-' => sub { da(); di(); di(); di(); di(); da(); stp(); },
    ';' => sub { da(); di(); da(); di(); da(); di(); stp(); },
    '/' => sub { da(); di(); di(); da(); di(); stp(); },
    '(' => sub { da(); di(); da(); da(); di(); stp(); },
    ')' => sub { da(); di(); da(); da(); di(); da(); stp(); }, # Not all have closing paragraph.
    '"' => sub { di(); da(); di(); di(); da(); di(); stp(); },
    '_' => sub { di(); di(); da(); da(); di(); da(); stp(); },
    '=' => sub { da(); di(); di(); di(); da(); stp(); },
    '+' => sub { di(); da(); di(); da(); di(); stp(); },
    '!' => sub { da(); di(); da(); di(); da(); da(); stp(); }, # Wikipedia & Heathkit but few others.
#   '!' => sub { da(); da(); da(); di(); stp(); },             # American landline but none other.
    '&' => sub { di(); stp(); di(); di(); di(); stp(); },
    '$' => sub { di(); di(); di(); da(); di(); di(); da(); stp(); },
    '@' => sub { di(); da(); da(); di(); da(); di(); stp(); },
    
    # Most common letters
    'A' => sub { di(); da(); stp(); },
    'B' => sub { da(); di(); di(); di(); stp(); },
    'C' => sub { da(); di(); da(); di(); stp(); },
    'D' => sub { da(); di(); di(); stp(); },
    'E' => sub { di(); stp(); },
    'F' => sub { di(); di(); da(); di(); stp(); },
    'G' => sub { da(); da(); di(); stp(); },
    'H' => sub { di(); di(); di(); di(); stp(); },
    'I' => sub { di(); di(); stp(); },
    'J' => sub { di(); da(); da(); da(); stp(); },
    'K' => sub { da(); di(); da(); stp(); },
    'L' => sub { di(); da(); di(); di(); stp(); },
    'M' => sub { da(); da(); stp(); },
    'N' => sub { da(); di(); stp(); },
    'O' => sub { da(); da(); da(); stp(); },
    'P' => sub { di(); da(); da(); di(); stp(); },
    'Q' => sub { da(); da(); di(); da(); stp(); },
    'R' => sub { di(); da(); di(); stp(); },
    'S' => sub { di(); di(); di(); stp(); },
    'T' => sub { da(); stp(); },
    'U' => sub { di(); di(); da(); stp(); },
    'V' => sub { di(); di(); di(); da(); stp(); },
    'W'  => sub { di(); da(); da(); stp(); },
    'X'  => sub { da(); di(); di(); da(); stp(); },
    'Y'  => sub { da(); di(); da(); da(); stp(); },
    'Z'  => sub { da(); da(); di(); di(); stp(); },

    '0'  => sub { da(); da(); da(); da(); da(); stp(); },
    '1'  => sub { di(); da(); da(); da(); da(); stp(); },
    '2'  => sub { di(); di(); da(); da(); da(); stp(); },
    '3'  => sub { di(); di(); di(); da(); da(); stp(); },
    '4'  => sub { di(); di(); di(); di(); da(); stp(); },
    '5'  => sub { di(); di(); di(); di(); di(); stp(); },
    '6'  => sub { da(); di(); di(); di(); di(); stp(); },
    '7'  => sub { da(); da(); di(); di(); di(); stp(); },
    '8'  => sub { da(); da(); da(); di(); di(); stp(); },
    '9'  => sub { da(); da(); da(); da(); di(); stp(); },

    # Esperanto characters. Work undelimited when *LANG=EO*
    'Cx' => sub { da(); di(); da(); di(); di(); stp(); },
    'Gx' => sub { da(); da(); di(); da(); di(); stp(); },
    'Hx' => sub { da(); da(); da(); da(); stp(); },        # http://eo.wikipedia.org/wiki/Morsa_kodo
    'Jx' => sub { di(); da(); da(); da(); di(); stp(); },
    'Sx' => sub { di(); di(); di(); da(); di(); stp(); },
    'Ux' => sub { di(); di(); da(); da(); stp(); },
       
    # Common prosigns. Require delimiting as *AA*, etc.
    # Some are redundant with punctuation.
    'CT' => sub { ctone2(); },                                      # custom tone
    'AA' => sub { di(); da(); di(); da(); stp(); },                          # 'End of line'
    'AL' => sub { di(); da(); di(); da(); di(); di(); stp();},               # Paragraph symbol
    'AR' => sub { di(); da(); di(); da(); di(); stp(); },                    # 'End of message' or '+' 
    'AS' => sub { di(); da(); di(); di(); di(); stp(); },                    # 'Wait'
    'BT' => sub { da(); di(); di(); di(); da(); stp(); },                    # 'Break' or 'Um, er, ah' or '='
    'CL' => sub { da(); di(); da(); di(); di(); da(); di(); di(); stp(); },  # 'Closing station'
    'KA' => sub { da(); di(); da(); di(); da(); stp(); },                    #  Start of message.
    'KN' => sub { da(); di(); da(); da(); di(); stp(); },                    # 'Over to named station'
    'NR' => sub { da(); di(); di(); da(); di(); stp(); },                    # 'Number(s) to follow'
    'SK' => sub { di(); di(); di(); da(); di(); da(); stp(); },              # 'End of contact'
    'SN' => sub { di(); di(); di(); da(); di(); stp(); },                    # 'Understood' or 'Sx' in Eo.
    'IMI'   => sub { di(); di(); da(); da(); di(); di(); stp(); },           # 'Huh?' or 'I say again'
    'SOS'   => sub { di(); di(); di(); da(); da(); da(); di(); di(); di(); stp(); },
    'ERROR' => sub { di(); di(); di(); di(); di(); di(); di(); di(); stp(); },
    'ATTENTION' => sub { da(); di(); da(); di(); da(); stp(); },
    'DOCALL' => sub { add_call(); },
    
    'NOOP' => sub { },
);

# Operations embeded in text, like so: *TONE=800* *WPM=20*
my %ops = (     
    'T1'   => sub { $tone1 = $_[0];      print "\tOkay! Tone1 = $_[0] Hz \n" if $v_flag; },
    'T2'   => sub { $tone2 = $_[0];      print "\tOkay! Tone2 = $_[0] Hz \n" if $v_flag; },
    'TA'   => sub { $amp_max = $_[0];    print "\tOkay! Amplitude = $_[0] % \n" if $v_flag; },
    'TD'   => sub { $duration = $_[0];   print "\tOkay! Duration = $_[0] ms \n" if $v_flag; },
    'TS1'  => sub { $sweep_rate1 = $_[0]; print "\tOkay! Sweep1 = $_[0] Hz/s \n" if $v_flag; },
    'TS2'  => sub { $sweep_rate2 = $_[0]; print "\tOkay! Sweep2 = $_[0] Hz/s \n" if $v_flag; },
    'TR'   => sub { $a_d_rate = $_[0];   print "\tOkay! A/D Ramp = $_[0] %/s \n" if $v_flag; },
    'SAMPN' => sub { $n = $_[0];         print "\tOkay! n = $_[0] %/s \n" if $v_flag; },
    'TONE' => sub { $tone = $_[0];       print "\tOkay! Tone = $_[0] Hz \n" if $v_flag; },
    'DIFF' => sub { $diff_10ths = $_[0]; print "\tOkay! Difficulty = $_[0] of 10 \n" if $v_flag; },
    'WPM'  => sub { $wpm = $_[0];        print "\tOkay! WPM = $_[0] \n" if $v_flag; },
    'LANG' => sub { $lang = lc($_[0]);   print "\tOkay! Language = $_[0] \n" if $v_flag; },
    'RAND' => sub { $rand_flag = $_[0];  print "\tOkay! Rand flag = $_[0] \n" if $v_flag; },
    'KOCH' => sub { $koch_flag = $_[0];  print "\tOkay! Koch flag = $_[0] \n" if $v_flag; },
    'INCR' => sub { $wpm_incr = $_[0];   print "\tOkay! Incr flag = $_[0] \n" if $v_flag; },
    'RAMP' => sub { $rampf = $_[0];      print "\tOkay! RAMP = $_[0] \n" if $v_flag; },
    'TAIL' => sub { $tail_op = $_[0];    print "\tOkay! TAIL = $_[0] \n" if $v_flag; },
    'CALL' => sub { $call_sign = $_[0];  print "\tOkay! CALL = $_[0] \n" if $v_flag; },
    'GAP'  => sub { $gap = $_[0];        print "\tOkay! GAP = $_[0] \n" if $v_flag; },
    'VERB' => sub { $v_flag = $_[0];     print "\tOkay! VERBose = $_[0] \n" if $v_flag; },
    'DISP' => sub { $d_flag = $_[0];     print "\tOkay! DISPlay = $_[0] \n" if $v_flag; },
    'NEXT' => sub { next_wav(); },
);

# Dit length is the master time unit, as follows:
# secs_per_min / PARIS_elements / words_per_min
my $dit_length  = 60 / 50 / $wpm * $sample_rate; 

my $pi        = ( 3.141593 ) * 2;
my $bits_sample = 16;
my $max_no    = ( 2**$bits_sample ) / 2;
my $details   = {
    'bits_sample' => $bits_sample,
    'sample_rate' => $sample_rate,
    'channels'  => 1,
};
my $nyquist_lim = ($sample_rate - 2) / 2;

my $write;

# Create a new, innumerated ouput *.wav file.
sub new_wav {
    my $head_flag = shift;
    ++$wav_cnt;
    my $wav_sfx = sprintf("%03s",$wav_cnt);
    my $wav         = new Audio::Wav;
    $wav_path = $txt_path;
    $wav_path =~ s/\.txt$/_$wav_sfx\.wav/;
    $write = $wav->write( "$wav_path", $details ); 
    mk_tag_hash();
    $write->set_info( 'name' => "$tags{'name'}" );
    $write->set_info( 'genre' => "$tags{'genre'}" );
    $write->set_info( 'comment' => "Generated $tags{'generator'}" );
    $wpm += $wpm_incr;

    # When incrementing WPM, incrementally approach 100% difficulty.
    $diff_10ths *= 1 + $wpm_incr / $wpm;
    $diff_10ths = 10 if $diff_10ths > 10;

    new_xml() if $xml_flag;
    add_head($wav_path) if $head_flag;
}

# Prepare to write also as Morse mail format in XML.
sub new_xml { 
    $xml_path = $wav_path;
    $xml_path =~ s/wav$/xml/;
    if ( open XML_OUT, ">$xml_path" ) {
        print "Okay! File $xml_path opened for writing.\n" if $v_flag;        
        print XML_OUT "<morse>\n";
    }
    else { 
        $xml_flag = 0;
        print "Oops! Cannot open $xml_path for writing.\n"; 
    }
}

sub close_xml { 
    print XML_OUT "</morse>";
    close XML_OUT;
}

# Return Date Time Group in ISO 8601 approved fashion.
sub current_DTG {
    my ($secs, $msecs) = Time::HiRes::gettimeofday();
    my ( $sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst ) = gmtime($secs);
    return sprintf( "%04d-%02d-%02d %02d:%02d:%02d.%5s Z", $year + 1900, $mon + 1, $mday, $hour, $min, $sec, $msecs );
}

# Provide basic default info tags.
sub mk_tag_hash {
    $tags{'name'} = (split /\//, $txt_path)[-1];
    $tags{'name'} =~ s/\.txt$//;
    $tags{'name'} =~ s/_/ /g;
    $tags{'name'} = ucfirst $tags{'name'};
    $tags{'date_time'} = current_DTG();
}

# Called by certain punctuation to break *.wav files
# into managable sizes at apporopriate points.
sub next_wav {
    add_tail();
    print("Done writing file: $wav_path \n\n") if $v_flag;
    $write->finish();
    close_xml() if $xml_flag;
    oggify();
    new_wav(1);
}

# Calculate miliseconds for Morse mail format.
sub xml_ms {
    my $i = shift;
    $i /= $wpm / 50 / 60; # Convert to mS as  WPM / PARIS / Sec

    # Tone is +N mS, silencd is -N mS.
    # Concatinate mS if not alternating tone/silence/tone/silence etc.
    if ( @xml_buff 
         && (
              ( $i < 0 && $xml_buff[-1] < 0 )
              ||  
              ( $i > 0 && $xml_buff[-1] > 0 )
            )
       ) { 
        $xml_buff[-1] += $i;
    }

    else { push @xml_buff, $i }
}

# Write out a line of morse as text.
sub xml_write {
    if (@xml_buff) {
        my $gmt = current_DTG;
        print XML_OUT qq|\t<line id="$_[0]" gmt="$gmt">\n\t\t|;
        while ( my $i = shift @xml_buff ) {
            print XML_OUT sprintf("%+d", $i);
       }
        print XML_OUT "\n\t</line>\n";
    }
}

# A dit element.
sub di {
    xml_ms(1) if $xml_flag;
    cw($dit_length);
    gap();
}

# A dah element.
sub da {
    xml_ms(3) if $xml_flag;
    cw( $dit_length * 3 );
    gap();
}

# Generate CW tone of required length
sub cw {
    print "\tOkay! Custom Tone\n" if $v_flag;
    my $j = int( $rampf * ($sample_rate / 1000) );  # convert ramp in ms to #samples
    if ($j == 0) { $j = 1 }                         # can't have zero rise time.
    for ( 0 .. $_[0] ) {
        my $y = $max_no;
        if ($_ < $j) { $y *= $_ / $j }              # Rise time.
        elsif ($_[0] - $_ < $j) { $y *= ($_[0] - $_) / $j } ; # Fall time.
        $write->write( $y * sin( $pi * $_ / $sample_rate * $tone ) );
    }
}

#########
# Generate generic 2-tone sequence of specified length
# uses: $duration, $n, $sample_rate, $amp_max, $max_no, $a_d_rate, $sweep_rate1,
#  $sweep_rate2, $pi, and $nyquist_lim.

sub ctone2 {
print "sw1 = $sweep_rate1 \n" if $v_flag;
   my $o = 0;
   my $t = 0;
   my $y = $duration * $sample_rate / 1000;
   my $z = ( $amp_max * $max_no ) / 100;
   my $zz = ($a_d_rate * $max_no ) / 100;     # ramp rate in LSBs per sample
   $zz /= $sample_rate;
   for ( $n .. ($n + $y) ) {
         # write tone first:
        $t = $_ / $sample_rate;
        $o = ($z * sin( ($pi * $tone1 * $t) + ($pi * $sweep_rate1 * $t * $t / 2) )) + ($z * sin( ($pi * $tone2 * $t) + ($pi * $sweep_rate2 * $t* $t / 2) )) ;
        $write->write( $o );
        print "  $n , $o \n" if $d_flag;      # If d_flag, display n,f(t) for capture of waveform
	 # now update ramp/sweep params:
        $z += $zz;                            # Rise time.
        if ( $z < 0 ) { $z = 0; }
        if ( $z > $max_no ) { $z = $max_no; $zz = 0; }
        if ( $sweep_rate1 != 0 ) {            # terminate Fsweep1
           if ( (($sweep_rate1 * $_ / $sample_rate) + $tone1) > $nyquist_lim ) {
               $tone1 = $nyquist_lim; $sweep_rate1 = 0;
           }
           if ( (($sweep_rate1 * $_ / $sample_rate) + $tone1) < 0 ) {
               $tone1 = 0; $sweep_rate1 = 0;
           }
        }
        if ( $sweep_rate2 != 0 ) {            # terminate Fsweep1
           if ( (($sweep_rate2 * $_ / $sample_rate) + $tone2) > $nyquist_lim ) {
               $tone2 = $nyquist_lim; $sweep_rate2 = 0;
           }
           if ( (($sweep_rate2 * $_ / $sample_rate) + $tone2) < 0 ) {
               $tone2 = 0; $sweep_rate2 = 0;
           }
        }
        $n++;                                 # update $n for continuity...
   }
    #if sweep in effect, update final tone frequency
   if ( $sweep_rate1 != 0) {
         $tone1 = ($sweep_rate1 * $duration / 1000) + $tone1;
   }
   if ( $sweep_rate2 != 0) {
         $tone2 = ($sweep_rate2 * $duration / 1000) + $tone2;
   }
   $amp_max = $z * 100 / $max_no;
}

# Make space between dits.
sub gap {
    xml_ms(-1) if $xml_flag;
    for my $pos ( 0 .. $dit_length ) {
        $write->write(0);
    }
}

# Make space between chars.
sub stp {
    xml_ms(-3) if $xml_flag;
    my $stp_length = int( $dit_length * 3 * 10 / $diff_10ths );
    for my $pos ( 0 .. $stp_length ) {
        $write->write(0);
    }
}

# Assemble special character key from all between paired asterisks.
# Return key and advanced pointer.
# Example special characters: *AR*, *KN*, *SK*, *Oops!*
sub special_char {
    my ($aref, $i) = @_;
    my $char = '';
    my $j;
    ++$i; # Skip since '*' not defined in Morse code.

    # Assemble key for presumed special char.
    for ( $j = $i; $j <= $#$aref; ++$j ) { 
        last if $aref->[$j] eq '*';
        $char .= $aref->[$j];
    }    

    # Return key and new pointer.
    if ( defined($morse{"$char"}) ) {
        print "\tOkay! Special char: *$char* \n" if $v_flag;
        return ($char, $j);
    }
    else { 
        if ( defined($ops{"$char"}) || $char =~ /=/ ) { 
            special_op("$char");
            return ('NOOP', $j); # Undefined char will be skipped.
        }
        elsif ( $j - $i > 16 ) {
            print "\tOops! Lone asterisk '*' found in text. Replaced by '(STAR)' text. \n" if $v_flag;
            &{$morse{'('}}; 
            &{$morse{'S'}};  
            &{$morse{'T'}};  
            &{$morse{'A'}};  
            &{$morse{'R'}};  
            &{$morse{')'}};
            return ('NOOP', $i + 1);
        }
        else {
            print "\tOops! Undefined char: *$char* found in text. Replaced by 'error' code. \n" if $v_flag;
            return ('ERROR', $j);
        }
    }    
}

# Perform embeded special operations such as *foobar* from input text.
sub special_op {
    my ($op,$arg) = split /=/, $_[0];
    if ( defined($ops{$op}) ) {       
        $ops{$op}->($arg)
    }
    else {
        print "\tOops! Skipping undefined special op: *$op* \n" if $v_flag;
    }  
}

# Convert string to Morse code.
sub morsify {
    my $line_cnt = shift;
    my $char_cnt = 0;
    for ( my $i = 0 ; $i <= $#_ ; ++$i ) {
        my $char = $_[$i];

        # Give indication of output
        print "Line $line_cnt: " . ( join('', @_) . "\n" ) if $v_flag && $i == 0;

        if ($char eq '*') { ($char, $i) = special_char(\@_, $i) }

        # Traktu Esperanton laux la iksa sistemo.
        if ($lang eq 'eo') {
            no warnings; # Look-ahead returns uninitialized at EOS!
            next if $char =~ /x/i && $i > 0 && $_[$i - 1] =~ /C|G|H|J|S|U/; # When to skip X.
            $char .= 'x' if $_[1 + $i] =~ /x/i; # When to Esperantize.
        }

        next unless defined( $morse{"$char"} );
        &{ $morse{"$char"} };
        ++$char_cnt if $char !~ /(NOOP|\s)/; # Track for &add_head triggering.

        # The Audio::Wav module buffers output so cannot check immediately that
        # output file has not been yanked away by an external delete command.
        # But such a delete can result in near-endless loop of errors. So exit.
        my @file_stat = stat $wav_path;
        if ( $line_cnt > 2 && $char_cnt > 10 ) {
            if ( $file_stat[7] == 0 ) { 
                die "Throughput stopped at line $line_cnt, char $char_cnt "
                    . "because file '$wav_path' deleted externally.\n";
            }  
            elsif ( $file_stat[7] / 1024**2 > $max_wav_mb ){
                no warnings; # Look-ahead returns uninitialized at EOS!

                # Break multi-file output at appropriate end-of-phrase punctuation.
                my $q = "\042"; # Quote as HEX so editor won't foobar syntax highlighting.
                next_wav() if "$char$_[1 + $i]" =~ /(\.$q)|(\?$q)|(!$q)|(\. )|(\? )|(! )|(\.$)|(\?$)|(!$)/i;
            } 
        }
    }
    # For 1st line of 1st file, if no chars sent, then assume
    # that 1st line is tags only. Add head info.
    if ( $line_cnt == 1 && $char_cnt == 0) { 
        add_head($wav_path);
    }

    xml_write($call_sign) if $xml_flag;
}

# Covert *.wav to *.ogg for good quality, smaller files.
sub oggify {
    if ( $oggenc_path && $codec =~ /ogg/i ) {
        my $ogg_cmd = 
             qq| $oggenc_path -q 3 $wav_path |
           . qq| -t "$tags{name}" |
           . qq| -G "$tags{genre}" |              
           . qq| -d "$tags{date_time}" |        
           . qq| -c "generator=$tags{generator}" |
           . qq| -c "wpm=$wpm" |          
           . qq| -c "diff=$diff_10ths" |
        ;
        `$ogg_cmd`;
        unlink $wav_path; # Lose the *.wav
    }
}

# A split on // will separate cxapelito base char from its 'x'
# This sub will repair an Esperanto iksa sistemo pair after
# a split on // by re-associating the 'x' to its base and
# replacing the 'x' with a space.
sub fix_iksa_split { 
    for (0 .. $#_ - 1) { 
      if ( $_[1+$_] =~ /x/i ) { 
        $_[1+$_] = '';  # Teleport orphan 'x' from isolation...
        $_[$_] .= 'x';  # ...re-uniting it to its cxapelito.
      }
    } 
    return @_;
}

# Modified from fisher-yates_shuffle from the Perl Cookbook, 4.17. 
sub shuffle_chars { 
    my @chars = split //, $_[0]; 
    @chars = fix_iksa_split(@chars) if $lang eq 'eo';
    @chars = @chars[0 .. 100] if $#chars > 100;
    my $i;
    for ($i = @chars; --$i;) {
        my $j = int rand ($i+1);
        next if $i == $j;
        @chars[$i,$j] = @chars[$j,$i];
    }
    return join '', @chars;
}

# Create a long random string from a file line having as few as 2 chars.
# Inspired by the Koch method of CW training.
# TO DO: Fix for tracking Esperanto.
sub kochify {
    my $wpm_old = $wpm;
    my $diff_10ths_old = $diff_10ths;
    $wpm = 25 if $wpm < 25;
    $diff_10ths = 5 if $diff_10ths < 5;
    $_ = shuffle_chars($_); 
    until (length $_ >= 100) {
        $_ .= shuffle_chars($_);    
    }
    $_ =~ s/^\s+//; # Lose spaces shuffled to leftmost of string.
    $wpm = $wpm_old;
    $diff_10ths = $diff_10ths_old;
    return "$_ ";
}

# Generate info for the head of each audio file.
sub msg_head { 
    if ($tail_op == 1) {
        my $file_name = shift;
        $file_name =~ s/.*[\\|\/]//; # Keep only name, not path.
        $file_name =~ s/wav$/ogg/ if $codec =~ /ogg/i; 
        my $info = qq|File = "$file_name"\n|;
        $info .= sprintf "Speed = %2.3f WPM ", $wpm;
        if ($diff_10ths < 10) {
            $info .= sprintf("chars @ %2.3f WPM spacing\n", $wpm * $diff_10ths / 10 );
        }
    	$info .= "Date = $tags{'date_time'}\n";
        print "$info";
    	$info =~ s/\n/; /g; # Punctuate for CW.
    	return prep_txt(" CQ DE $call_sign $info *KA* ");
    }
}

# send call_sign.
sub do_call { 
    return prep_txt("$call_sign");
}

# Generate info for the tail of each file.
sub msg_tail {
    if ($tail_op == 1) {
        return uc(" *AR* DE $call_sign *SK* *CL* ");
    }
    else {return uc(" ")}
}

# Subs to add head and tail to each audio file.
sub add_head { morsify( 0, (split //, msg_head($_[0]) ) ) }
sub add_tail { morsify( 0, (split //, msg_tail() ) ) }
sub add_call { morsify( 0, (split //, do_call() ) ) }

# Get text ready for morsification.
sub prep_txt {
    my $txt = shift;
    $txt =~ s/^\s*//g;      # Lose leading whitespace.
    $txt =~ s/\s+/ /g;      # Swap whitespace, compressing plural.
    $txt =~ tr/[]/()/;      # Morse has no square bracket.
    $txt =~ tr/{}/()/;      # Morse has no curly brace.  
    return uc($txt);        # Morse hash keys are upper case.
}

# Read input text, convert to audio file(s).
if ( open INFILE, "<$txt_path" ) {
    print "Running ct.pl version $VERSION \n";
    my $line_cnt = 0;
    new_wav(0);
    while (<INFILE>) {
        ++$line_cnt;               # For verbose reporting.
        next if $_ =~ /^\s+$/;     # Skip blank lines.
        $_ = prep_txt($_);
        
        # Test first because...
        # Shuffler chokes on 1-elem arrays, and...
        # Shuffler will mangle embeded *-delited special ops.
        if ( (length $_ > 1) && ($_ !~ /\*/) ) {
            $_ = kochify($_) if $koch_flag ;
            $_ = shuffle_chars($_) if $rand_flag;
            $_ =~ s/\s+/ /g;
        }
        morsify( $line_cnt, (split //, $_) );        
    }
    xml_write($call_sign) if $xml_flag;
    add_tail();
    $write->finish();
    close_xml() if $xml_flag;
    oggify();
    print "All done.\n";
}
else { print "Oops! Cannot open infile '$txt_path': $! \n" }

__END__

=head1 NAME

Morse Code Text-to-Audio Converter

=head1 SYNOPSIS

C<perl gus_morse.pl --path /ram/message.txt --wpm 15 --diff 5 --tone 750 --samp 8000 --codec ogg --rand --verbose>

=head1 DESCRIPTION

Reads in text file, writes out Morse code as audio file. Writes initially to C<*.wav> then converts.

=head1 MODULES USED

C<Getopt::Long>

C<Audio::Wav>

C<Config>

=head1 COMMAND LINE OPTIONS

Control behavior of text-to-audio conversion using these options.

=head2 --p[ath]
 
Valid system file path to input text file. Audio output file(s) will be written to same directory, with extensions for inumeration and codec. Default = './morse.txt'

=head2 --w[pm] 

Character speed as words-per-minute...independent of character spacing. Default = 15.0.

=head2 --d[iff] 

Difficulty level. Character spacing = wpm when diff = 10. Wider spacings at dif < 10. Default = 5.0

=head2 --t[one] 

Tone of CW characters in Hz. Default = 750

=head2 --m[ax] 

Once output file exceeds this limit in MB, a new file will split off at next major punctuation. Default = 200

=head2 --i[ncr]

For each new file from 2nd thru Nth spit off by exceeding --max value, increase --wpm by this ammount. Default = 0;

=head2 --s[amp]

Sampling rate of the C<*.wav> file in Hz. Default = 8000.

=head2 --c[odec] 

Which audio codec (format) as final output? Default = C<*.wav> Other formats supported (non-Win32 only): C<*.ogg>

=head2 --l[ang] 

Language of input C<*.txt> file and subsequent Morse code charset.

=over

=item en 

English via US ASCII (Default)

=item eo

Esperanto laux la iksa sistemo. 

=item others? 

To be supported later...maybe.

=back

=head2 --r[and]    

Randomly shuffle all characters in each line. This will convert any text to random practice. When -l[

=head2 --k[och]    

Like --r[and] except that the character count for each line will be expanded by adding tupples of all those provided. This is to facilitate the Koch method of teaching Morse code where only one each from a very small subset of characters are provided on a given line. It will also increase the character speed to 25 WPM if below that and the difficulty to 5 if below that. 

=head2 --x[ml]

XML flag triggers output of text file encoding Morse code in mS. For example: '+200' for a dit; '+600' for a dah; '-200' for intra-character space; '-700' for inter-character space, etc. This is an experimental feature to generate throughput for a yet-to-be written, cross-platform alternative to the Win32-only softwares: Morse Mail and CWCom.

=head2 --v[erbose] 

Verbosity flag provides verbose feedback

=head2 --h[elp] Help

Display basic help/usage message.

=head1 SPECIAL CHARACTERS 

Converter will parse input text for *-delimited special characters and prosigns. Examples: *AL* *AR* *SK* *KN* *BT* *SOS* *ERROR*

=head1 UNOFFICIAL CHARACTERS

Converter will substitute non-offical characters for nearest common equivalent: Curley braces and brackets become parentheses; Exclamation point becomes the Wikipedia/Heathkit variant rather than the American lanline variant (inter-changable within the script).

=head1 EMBEDED OPERATORS

Converter will parse input text for *-delimited special operators. These have the same effect as their CLI arg equivalents, but may be embeded mid-stream in the text file so as to take effect mid-stream during the playout. Use them to simulate QSO between multiple stations.

=head2 Regular Ops

These have same effect as their CLI-arg equivalents. Examples: *TONE=775* *WPM=20.6* *DIFF=8.5* *LANG=EN* *LANG=EO*

=head2 *RAND=1* and *KOCH=1*

These differ from their CLI-arg equivalents in requiring either a 0 or a 1 as integer arguments. On the CLI they are flags, always equal to 1. As embedded operators they may be toggled on and off.

=head2 *NEXT*

This op has no CLI-arg equivalent. Its function is to trigger a break between consecutive output files.

=head1 PER-FILE ADD-ONS

Each new file is embellished with add-on head and tail charcter streams. This is to provide some exercise in numeric content. Otherwise there is likely to be little or none since this entire script is mainly aimed at transcoding whole books into CW.

=head2 HEAD

The head will announce which file is beginning, its speeds and time of creation, as exampled below. These can be changed only with the Perl script itself. The call sign, for instance, is a variable.

CQ DE KY8D FILE = "PG_765_MORSE_068.ogg"; SPEED = 22.824 WPM CHARS @ 18.943 WPM SPACING; DATE = 2006-03-05 17:46:22.415539 Z KA

=head2 TAIL

The tail will announce the end of transmission, as exampled below.

AR DE KY8D SK CL

=head1 AUTHOR

Gan Uesli Starling F<gan@starling.us>

=head1 COPYRIGHT

Copyright (c) 2006, Gan Uesli Starling. All rights reserved.

This program is free software; you can redistribute it and/or modify it under the same terms as Perl itself.

=head1 SCRIPT CATEGORIES

Convert 

