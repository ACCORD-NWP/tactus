#!/usr/bin/perl -w
#
# OBSOUL DECODING, FILTERING AND MERGING
#
# THIS SCRIPT JOINS OBSOUL FILES SPECIFIED IN THE LIST INTO ONE DESTINATION FILE
# WHILE CORRUPTED RECORDS AND ALL DUPLICATED RECORDS (HAVING THE SAME STATION ID)
# ARE EXCLUDED AND CHECKED FOR POSSIBLE LAT/LON DISCREPANCY. NUMBER OF AFFECTED
# RECORDS IS REPORTED.
#
# MAIN FEATURES:
#
# a) only the identical stations within the same observation type (but in
#    whichever input file or within the single file) are excluded => this
#    is to keep for instance both SYNOP and TEMP observations for the same
#    station ID in the final output (e.g. if you have two or more 11035
#    SYNOP records and two or more 11035 TEMP records somewhere in the input
#    files, there will be always saved just two records in the result: the first
#    one 11035 SYNOP observation and the first one 11035 TEMP observation)
#
# b) there is no filtering for AMDAR data, since the planes are usually moving ;-)
#    and that's why the same ID is present many times with different lat/lon positions
#    (which is perfectly OK of course) => all of them will be kept in the output file
#
# c) in case some observations are duplicated within the input files or in one
#    single file (and it is not the case related to the first two points) => the one
#    which appears first is accepted and the rest is skipped (i.e. if you prefer some
#    given source of observations, just put it higher in the list of all input files)
#
# d) multiple time-slots for the observations in input files are also supported
#    (network time will be converted: H, HH, HMMSS, HHMMSS -> HHMMSS)
#
# e) incorrect/corrupted records are ignored
#
# f) whitelist can be used as an option (FORMAT: obstype:codetype:id, eg. 2:10031145:M1ed66b)
#
# g) data for each record are printed in fixed-format on a single line (required since cy46)
#    header A: printf OUT ("%4d %2d %9d %9.5f %10.5f %12s ", @STACK);
#    header B: printf OUT ("%8d %06d %12.5E %5d %7d %7d ",   @STACK);
#    body  Nx: printf OUT (" %3d %12.5E %12.5E %12.5E %11d", @STACK);
#
# Use env variable CNF_DEBUG to see the detailed output listing (can be very long one ;-)
# (e.g. "export CNF_DEBUG=1" before running the merging script or use -v switch)
#
# ARGUMENTS:
#
#  (mandatory arguments)
#  -obsoul <new_merged_obsoul_file>
#  -files  <list_of_original_files>
#
#  (optional arguments)
#  -twindow   <time_window_in_seconds>
#  -whitelist <file_with_selected_IDs>
#
#  (optional switch)
#  -verbose
#
# EXAMPLE:
#
#  obsoul_merge.pl -o obsoul.merged -f list.txt [-t 3600] [-v]
#
# where list.txt contains full paths to the obsoul
# files (one per line) which are going to be merged
#
# created:  13-04-2011
# modified: 23-03-2012 - (header decoding, reading record by record)
#           08-06-2015 - bugfix: not-skipped duplicated records when rounded lat or lon was zero
#                      - added time window ($twindow) for accepting the observations
#           13-06-2016 - time window can be supplied as an optional argument (-twindow or -t)
#           28-05-2021 - whitelist can be supplied as an optional argument (-whitelist or -w)
#                      - gzip-ed obsoul files are supported in the list of input files (.gz)
#           04-06-2021 - data are printed in fixed-format on a single line (cy46 req.)
#           16-06-2021 - time-slot from the first non-empty input file is used
#           16-07-2021 - fix for zero body rows
#
# version: 07
#
# author:   M. Bellus (martin.bellus@gmail.com)
######################################################################################


################
# used modules #
################

use POSIX;
use Getopt::Long;


############
# settings #
############

# lat/lon precision for duplicated records position control
# 10 for AB.C, 100 for AB.CD, 1000 for AB.CDE, etc.
# (filtering itself is not affected by this choice)
$precision = 100;

# default value of observation time window [s]
$twindow = 3*60*60;


#######################
# get input arguments #
#######################

my %args = ();

# define all commandline options
GetOptions(
   "obsoul=s"    => \$args{obsoul},
   "files=s"     => \$args{files},
   "whitelist=s" => \$args{whitelist},
   "twindow:i"   => \$args{twindow},
   "verbose!"    => \$args{verbose},
);

# automatically turn on CNF_DEBUG if verbose mode
if ( $args{verbose} ) {
   $ENV{CNF_DEBUG} = 1;
}

# check whether mandatory options were used (if not => die ;-)
if ( $args{obsoul} and $args{files} ) {
   $OBSOUL_FILE = $args{obsoul};
   $LISTFILE    = $args{files};
} else {
   die "\n(!) USE: $0 -o <new_merged_file> -f <list_of_input_files> [-t <seconds>] [-w <whitelist>] [-v]\n\n";
}

# change time window if supplied by the argument
if ( $args{twindow} ) {
   $twindow = $args{twindow};
}

# get and read whitelist if supplied by the argument
if ( $args{whitelist} ) {
   $wlsize = 0;
   open(IN, $args{whitelist}) or die "\n(!) Whitelist: $args{whitelist} doesn't exist\n\n";
   print "\nREADING WHITELIST: $args{whitelist}\n";
   while ( defined( $wid = <IN> ) ) {
       chomp($wid);
       $wid =~ s/\s+$//;
       $wid =~ s/^\s+//;
       if ( $wid =~ /^(\d):(\d+):([a-zA-Z0-9]+)$/ ) {
          $whitelist{$1}{$2}{$3} = 1;
          $wlsize++;
       }
   }
   close(IN);
   print "...number of IDs in whitelist: " . $wlsize . "\n";
}

# check input file existence (the list of obsouls to be merged)
if (! -e "$LISTFILE" ) {
   die "\n(!) Input file: $LISTFILE doesn't exist\n\n";
}


########################
# processing the files #
########################

print "\nMERGING OBSOULS: (time window is $twindow s)\n\n";

# clock counter
$starttime = time();

# read the content of FILELIST
open(LIST, "$LISTFILE") or die "\n(!) Can't open LISTFILE: $LISTFILE\n\n";
@obslist = <LIST>;
close LIST;

# open output file for the merged obsoul data
open(OUT, ">$OBSOUL_FILE") or die "\n(!) Can't write to output file: $OBSOUL_FILE\n\n";

# initialization of global variables
%LAT         = ();
%LON         = ();

# loop through all input obs files (F)
FILE:
for ( $F=0 ; $F<=$#obslist ; $F++ ) {

   chomp($obslist[$F]);
   $file = $obslist[$F];
   $file =~ s/^.*\///;    # remove file path

   $obs_type{$F} = (split(/_/, $file))[1];

   # unzip input obs file (F) if necessary
   if ( $file =~ s/(\.gz)$// ) {
      $err = system("/bin/gunzip -c $obslist[$F] > $file");
      if (! $err) {
         $obslist[$F] = $file;
      } else {
         die "\n(!) Can't unzip input obsoul file: $obslist[$F]\n\n";
      }
   }
   if ( $obs_type{$F} eq "taw"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "ship"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "ita"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "slf"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq ""   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "metar"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "hydro"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "hydroschnee"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "bufrtemp"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "vienna"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "czech"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "schiwm"  ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "1_netatmo"  ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "metar"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "klagenfurt"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "GPS1"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "GPS2"   ) { $obs_type{$F} = 1 };
   if ( $obs_type{$F} eq "amdar" ) { $obs_type{$F} = 2 };
   if ( $obs_type{$F} eq "MODES" ) { $obs_type{$F} = 2 };
   if ( $obs_type{$F} eq "2_ehs_nl" ) { $obs_type{$F} = 2 };
   if ( $obs_type{$F} eq "pilot" ) { $obs_type{$F} = 6 };
   if ( $obs_type{$F} eq "wka" ) { $obs_type{$F} = 6 };
   if ( $obs_type{$F} eq "wka_avg" ) { $obs_type{$F} = 6 };
   if ( $obs_type{$F} eq "sodar" ) { $obs_type{$F} = 6 };
   if ( $obs_type{$F} eq "sodar2" ) { $obs_type{$F} = 6 };
   if ( $obs_type{$F} eq "tower" ) { $obs_type{$F} = 6 };
   if ( $obs_type{$F} eq "6_qc" ) { $obs_type{$F} = 6 };
   if ( $obs_type{$F} eq "temp" ) { $obs_type{$F} = 5 };
   if ( $obs_type{$F} eq "MF" ) { $obs_type{$F} = 4 };
   if ( $obs_type{$F} eq "pseudosounding" ) { $obs_type{$F} = 5 };
   if ( $obs_type{$F} eq "mrar_si") { $obs_type{$F} = 2 };
   if ( $obs_type{$F} eq "mrar_cz") { $obs_type{$F} = 2 };
   # read obs file
   open(OBS, "$obslist[$F]") or die "\n(!) Can't open input obsoul file: $obslist[$F]\n\n";
   print "=> READING FILE ($F): $obslist[$F]\n";

   # loop through all lines in this file
   LINE:
   while ( defined( $line = <OBS> ) ) {
      chomp($line);

      # scan observation validity (time-slot)
      if ( $line =~ /^\s*(\d{8})\s+(\d+)\s*$/ ) {

         # get validity and network time
         ($date, $NT) = ($1, $2);

         # convert network time (H, HH, HMMSS, HHMMSS -> HHMMSS)
         if ( $ENV{CNF_DEBUG} ) {
            print "   NT(orig): $NT\n";
         }
         $NT = &convert_time($NT);
         if ( $ENV{CNF_DEBUG} ) {
            print "   NT(conv): $NT\n\n";
         }

         # time stamp
         $year  = substr($date, 0, 4);
         $month = substr($date, 4, 2);
         $day   = substr($date, 6, 2);
         $hour  = substr($NT, 0, 2);
         $min   = substr($NT, 2, 2);
         $sec   = substr($NT, 4, 2);
         $TIME{$F} = mktime($sec, $min, $hour, $day, $month -1, $year-1900);

         # set actual time-slot according to input file
         $timeslot = $date . $NT;

      } else {

         # check date and time of the observation
         if ( ! defined( $timeslot ) ) {
            die "\n(!) Missing date and time of the observation\n";
         }

         # get rid of spaces surrounding station ID (if present)
         $line =~ s/\'\s*(\w+)\s*\'/\'$1\'/;

         # get rid of leading and trailing spaces
         $line =~ s/^\s+//;
         $line =~ s/\s+$//;

         # parse all data (hash: %DATA, key: $F:$timeslot, value: array of all values)
         push(@{$DATA{"$F:$timeslot"}}, split(/\s+/, $line));
      }
   }

   # close input file
   close OBS;
}

# OBSOUL header positions
$OBS_position = 1;  # obs type
$COD_position = 2;  # code type
$LAT_position = 3;  # station lat
$LON_position = 4;  # station lon
$IDS_position = 5;  # station ID
$DDD_position = 6;  # yyyymmdd of the observation
$TTT_position = 7;  # hhmmss of the observation
$BOD_position = 9;  # number of bodies following the header

# number of body items
$body_items = 5;

# statistics initialization
$record_number_index = 0;
$wrong_date          = 0;
$wrong_type          = 0;
$wrong_latlon        = 0;
$wrong_duplicated    = 0;
$wrong_record        = 0;
$wl_skipped          = 0;

# loop through all saved files and timeslots
if ( $ENV{CNF_DEBUG} ) {
   print "DATA PROCESSING:\n";
}
foreach $key ( sort( keys( %DATA ) ) ) {

   # position index initialization
   $single_data_index   = 0;

   # file number and timeslot
   ($F, $timeslot) = split(/:/, $key);

   # observation date and time (reference)
   $date = substr($timeslot,0,8);
   $NT   = substr($timeslot,8,6);

   # write observation date and time
   if ( ! $time_header ) {
      print OUT "    $date\t$NT\n";
      $time_header = 1;
   }

   # total number of data contained in this file
   $R = scalar( @{$DATA{$key}} );

   # reading data and parsing records
   RECORD:
   while ( $single_data_index < $R ) {

       # total number of data specified in the given observation record
       $N = $DATA{$key}[$single_data_index];

       # increment record number
       $record_number_index++;

       # write some information about given record
       if ( $ENV{CNF_DEBUG} ) {
          print "\n";
          print "   record number: $record_number_index\n";
          print "   total data:    $N\n";
       }

       # check for station ID
       if ( $DATA{$key}[$single_data_index+$IDS_position] =~ /\'(\w+)\'/ ) {
          $station_ID = $1;

          # whitelist FILTER applied
          if ( $args{whitelist} ) {
             $sID = $station_ID;
             $sID =~ s/^\s+//;
             $sID =~ s/\s+$//;
             $codetype = $DATA{$key}[$single_data_index+$COD_position];
             if ( defined($whitelist{$obs_type{$F}}{$codetype}) ) {
                if (! defined($whitelist{$obs_type{$F}}{$codetype}{$sID}) ) {

                   # write info
                   if ( $ENV{CNF_DEBUG} ) {
                      print "   station ID:    $station_ID -> (i) record skipped due to whitelist\n";
                   }

                   # skip this record
                   $single_data_index = $single_data_index + $N;
                   $wl_skipped++;
                   next RECORD;
                }
             }
          }

          if ( $ENV{CNF_DEBUG} ) {
             print "   station ID:    $station_ID\n";
          }

          # make sure the observation time is in format "hhmmss" (6 digits)
          $DATA{$key}[$single_data_index+$TTT_position] = sprintf("%6.6d", $DATA{$key}[$single_data_index+$TTT_position]);

          # check observation date and time (skip it if wrong)
          $YYYY = substr($DATA{$key}[$single_data_index+$DDD_position], 0, 4);
          $MM   = substr($DATA{$key}[$single_data_index+$DDD_position], 4, 2);
          $DD   = substr($DATA{$key}[$single_data_index+$DDD_position], 6, 2);
          $hh   = substr($DATA{$key}[$single_data_index+$TTT_position], 0, 2);
          $mm   = substr($DATA{$key}[$single_data_index+$TTT_position], 2, 2);
          $ss   = substr($DATA{$key}[$single_data_index+$TTT_position], 4, 2);
          $RTIME = mktime($ss, $mm, $hh, $DD, $MM -1, $YYYY-1900);

          if ( $RTIME < $TIME{$F} - 0.5*$twindow or $RTIME > $TIME{$F} + 0.5*$twindow ) {

             # wrong observation date and/or time
             if ( $ENV{CNF_DEBUG} ) {
                print "   (!) $station_ID => has wrong observation date/time";
                print " (file:$date-$NT record:$DATA{$key}[$single_data_index+$DDD_position]-$DATA{$key}[$single_data_index+$TTT_position])\n";
             }
             $wrong_date++;

             # skip this record
             $single_data_index = $single_data_index + $N;
             next RECORD;
          }

          # check observation type (skip it if wrong)
          if ( $DATA{$key}[$single_data_index+$OBS_position] != $obs_type{$F} ) {

             # wrong observation type defined in header
             if ( $ENV{CNF_DEBUG} ) {
                print "   (!) $station_ID => has wrong observation type (file:$obs_type{$F} record:$DATA{$key}[$single_data_index+$OBS_position])\n";
             }
             $wrong_type++;

             # skip this record
             $single_data_index = $single_data_index + $N;
             next RECORD;
          }

          # check position consistency (for duplicated records except AMDAR data)
          if ( defined( $LAT{"$station_ID:$obs_type{$F}:$timeslot"} ) and defined( $LON{"$station_ID:$obs_type{$F}:$timeslot"} ) and $obs_type{$F} != 2 ) {

             $rec_lat = int( $DATA{$key}[$single_data_index+$LAT_position] * $precision ) / $precision;
             $rec_lon = int( $DATA{$key}[$single_data_index+$LON_position] * $precision ) / $precision;

             if ( $LAT{"$station_ID:$obs_type{$F}:$timeslot"} != $rec_lat or $LON{"$station_ID:$obs_type{$F}:$timeslot"} != $rec_lon ) {

                    # different record position (but not AMDAR observation)
                    if ( $ENV{CNF_DEBUG} ) {
                       print "   (!) $station_ID => has wrong lat/lon (saved:$LAT{\"$station_ID:$obs_type{$F}:$timeslot\"}/$LON{\"$station_ID:$obs_type{$F}:$timeslot\"} record:$rec_lat/$rec_lon)\n";
                    }
                    $wrong_latlon++;

                    # skip this record
                    $single_data_index = $single_data_index + $N;
                    next RECORD;

             } elsif ( $LAT{"$station_ID:$obs_type{$F}:$timeslot"} == $rec_lat and $LON{"$station_ID:$obs_type{$F}:$timeslot"} == $rec_lon ) {

                    # exact record position
                    if ( $ENV{CNF_DEBUG} ) {
                       print "   (!) $station_ID => duplicated observation\n";
                    }
                    $wrong_duplicated++;

                    # skip this record
                    $single_data_index = $single_data_index + $N;
                    next RECORD;
             }

          } else {

             # save station lat/lon
             $LAT{"$station_ID:$obs_type{$F}:$timeslot"} = int( $DATA{$key}[$single_data_index+$LAT_position] * $precision ) / $precision;
             $LON{"$station_ID:$obs_type{$F}:$timeslot"} = int( $DATA{$key}[$single_data_index+$LON_position] * $precision ) / $precision;
          }

          # number of header items (one line)
          $header = 6;

          # number of body rows for this record
          $body_rows = $DATA{$key}[$single_data_index + $BOD_position];

          # check the length of bodies
          if ( $body_rows == 0 or ($N-2*$header)/$body_rows != $body_items ) {
             print "   (!) $station_ID => wrong length of bodies\n";
             $wrong_record++;

             # skip this record
             $single_data_index = $single_data_index + $N;
             next RECORD;
          }

          # header first row
          @STACK = ();
          for ( $j=$single_data_index ; $j<$single_data_index+$header ; $j++ ) {
              if ( $DATA{$key}[$j] =~ /\'(\w+)\'/ ) {

                 # station ID back to character 8 (required by bator)
                 $DATA{$key}[$j] = sprintf("\'%-8s\'", $1);
              }
              push(@STACK, $DATA{$key}[$j]);
          }
          printf OUT ("%4d %2d %9d %9.5f %10.5f %12s ", @STACK);
          $jnext = $j;

          # header second row
          @STACK = ();
          for ( $j=$jnext ; $j<$jnext+$header ; $j++ ) {
              push(@STACK, $DATA{$key}[$j]);
          }
          printf OUT ("%8d %06d %12.5E %5d %7d %7d ", @STACK);
          $jnext = $j;

          # record body
          @STACK = ();
          $items = 0;
          for ( $j=$jnext ; $j<$jnext+$N-2*$header ; $j++ ) {
              $items++;
              push(@STACK, $DATA{$key}[$j]);
              if ( $items == $body_items ) {
                 printf OUT (" %3d %12.5E %12.5E %12.5E %11d", @STACK);

                 # reset stack and counter
                 @STACK = ();
                 $items = 0;
              }
          }
          print OUT "\n";

          # shift data index to the next record
          $single_data_index = $j;

       } else {
          $wrong_record++;
          if ( $ENV{CNF_DEBUG} ) {
             print "   (!) station ID not recognised => scanning for the next record...\n";
          }
          while ( $DATA{$key}[$single_data_index+$IDS_position] !~ /\'(\w+)\'/ and $single_data_index < $R ) {
             $single_data_index++;
          }
       }
   }
}

# close output file
close OUT;

# total number of used records
$total_records = $record_number_index - $wrong_date - $wrong_type - $wrong_latlon - $wrong_duplicated - $wrong_record - $wl_skipped;
if ( $total_records > 0 ) {
   if ( $args{whitelist} ) {
      print "\n(i) Records skipped due to whitelist: $wl_skipped\n";
   }
   print "\n=> TOTAL RECORDS WRITTEN: $total_records\n\n";
} else {
   print "\n=> NO RECORDS WRITTEN!\n\n";
}

# write info if something went wrong
if ( $wrong_date > 0 ) {
   print "(!) Number of skipped records due to inconsistent date/time: $wrong_date\n";
}
if ( $wrong_type > 0 ) {
   print "(!) Number of skipped records due to inconsistent OBS type: $wrong_type\n";
}
if ( $wrong_latlon > 0 ) {
   print "(!) Number of skipped records due to inconsistent lat/lon: $wrong_latlon\n";
}
if ( $wrong_duplicated > 0 ) {
   print "(!) Number of skipped records due to duplicity: $wrong_duplicated\n";
}
if ( $wrong_record > 0 ) {
   print "(!) Number of skipped records due to corrupted header: $wrong_record\n";
}

# time consumed
print "\n=> FINISHED IN: ".(time()-$starttime)." secs\n\n";


# SUBROUTINE convert_time #######
#################################

# USE: &convert_time(time) --> HHMMSS

sub convert_time {

   # get the argument
   my($time) = @_;

   # private variables
   my($NT);

   # convert network time
   if ( length($time) == 6 ) {
      $NT = $time;
   } elsif ( length($time) == 5 ) {
      $NT = "0" . $time;
   } elsif ( length($time) == 4 ) {
      $NT = "00" . $time;
   } elsif ( length($time) == 2 ) {
      $NT = $time . "0000";
   } elsif ( length($time) == 1 ) {
      $NT = "0" . $time . "0000";
   } else {
      die "\n\n(!) wrong network time format\n\n";
   }

   # end of subroutine (successful)
   return($NT);
}


###################
# the end is near #
###################

# exactly here ;-)
exit(0);
