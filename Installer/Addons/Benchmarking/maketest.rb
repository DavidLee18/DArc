#!/usr/bin/env ruby
########################################################
### Benchmark process settings #########################
########################################################

# Directory used for creating temporary files (must contain the string temp)
$workdir = 'd:\temp'

# Amount of machine/VM RAM (used to decide whether it is worth caching files before compression)
$ramsize = 512*1024*1024

# Storing:
  ace_methods = ["-m0"]
  rar_methods = ["-m0"]
  arc_methods = ["-m0 -dm0"]
  _7z_methods = ["-mx0 -mf=off -mhcf=off -mhc=off"]
uharc_methods = ["-m0"]

#Strongest methods only
  _7z_methods = ["-mx9 -md=32m"]
  arc_methods = ["-m6x", "-m6", "-m6p"]
uharc_methods = ["-mx"]

# List of compression methods for the archivers under test ("--" is used to separate groups of similar methods in the report)
  ace_methods = ["-m1 -d64", "-m5"]
  sbc_methods = ["-m1 -b5", "-m2 -b15", "-m3 -b63"]
  rar_methods = ["-m1",  "-m2",  "-m3", "-m5 -mcd-", "-m5", "-m5 -mc14:128t"]   # omitted: "-m5 -mct-"
arc024_methods = ["-m1x",  "-m2xp",  "-m3xp",  "-m4xp",  "-m5xp",  "-m6xp",  "--",
                  "-m2p",  "-m3p",   "-m4p",   "-m5p",   "-m6p"
                 ]
arc030_methods = ["-m1x",  "-m2xp",  "-m3xp",  "-m4xp",  "-m5xp",  "-m6xp",  "--",
                  "-m2d",  "-m3d",   "-m4d",   "-m5d",   "-m6d",   "--",
                  "-m2p",  "-m3p",   "-m3pr",  "-m4p",   "-m5p",   "-m5pr",   "-m6p"
                 ]
arc031_methods = ["-m1x",  "-m2x",  "-m3x",  "-m4x",  "-m5x",  "-m6x",  "--",
                  "-m2",   "-m3",   "-m3r",  "-m4",   "-m5"
                 ]
arc036_methods = ["-m1x",  "-m2x",  "-m3x",  "-m4x",  "-m5x",  "-m6x",  "--",
                  "-m2",   "-m3",   "-m3r",  "-m4",   "-m5",   "-m5p"
                 ]
   arc_methods = ["-m1x",  "-m2x",  "-m3x",  "-m4x",  "-m5x",  "-m6x",  "--",
                  "-m2",   "-m2r",  "-m3r",  "-m3",   "-m4",   "-m5",   "-m6" ,  "--",
                  "-m6 -mcd-",      "-m5p",  "-m6p",  "-mdul0", "-mdul"
                 ]
arcext_methods= ["-mccm", "-mccmx", "-mlpaq", "-mdur", "-muda"]   # ["-mdul0", "-mdul", "-mccm", "-mccmx", "-mdur", "-mlpaq", "-muda"]
  _7z_methods = ["-mx1", "-mx3", "-mx5", "-mx7", "-mx9 -md=32m"]
uharc_methods = ["-mz",  "-m1",  "-m2",  "-m3",  "-mx"]
 bssc_methods = ["", "-t"]
WinRK_methods = ["rolz3"]  # ["fast", "normal", "rolz", "fast3", "normal3", "rolz3", "efficient"]  # omitted because they are terribly slow: "high", "max"
  sqc_methods = ["-uxx1", "-uxx5", "-uxx9"]
  sqc         = 'C:\Base\Tools\ARC\sqc\sqc'

#Command lines for WinRK (benchmarking does not actually work - the program waits for OK to be pressed)
#Besides, these settings cannot be used to benchmark compression of individual files
WinRK_add     = 'cmd /c start /w /min WinRK -create %archive -set profile %options -add +recurse * -apply -quit'
WinRK_test    = 'cmd /c start /w /min WinRK -open %archive -test    -quit'
WinRK_extract = 'cmd /c start /w /min WinRK -open %archive -extract -quit'


# List of archivers/packers under test: name, pack command, option for packing subdirectories, compression method options, test/extract commands
$archivers = [
#  ["WinRK 3.0.3"        , WinRK_add                                         , " " ,  WinRK_methods, WinRK_extract],
#  ["ARC 0.24"           , "Arc_0_24  a  -dsgen      %options %archive %file", "-r", arc024_methods, "Arc_0_24 t %archive"],  # "Arc_0_24 x %archive"],
#  ["ARC 0.25/0.30"      , "Arc_0_30  a  -dsgen      %options %archive %file", "-r", arc030_methods, "Arc_0_30 t %archive"],  # "Arc_0_30 x %archive"],
#  ["ARC 0.31"           , "Arc_0_31  a  -dsgen      %options %archive %file", "-r", arc031_methods, "arc      t %archive"],  # "arc      x %archive"],
#  ["ARC 0.32"           , "Arc_0_32  a  -dsgen      %options %archive %file", "-r", arc036_methods, "arc      t %archive"],  # "arc      x %archive"],
#  ["ARC 0.33"           , "Arc_0_33  a              %options %archive %file", "-r", arc036_methods, "arc      t %archive"],  # "arc      x %archive"],
#  ["ARC 0.36"           , "Arc_0_36  a              %options %archive %file", "-r", arc036_methods, "arc      t %archive"],  # "arc      x %archive"],
  ["ARC 0.40"           , "arc       a              %options %archive %file", "-r",     arc_methods, "arc      t %archive"],  # "arc      x %archive"],
  ["ARC externals"      , "arc       a              %options %archive %file", "-r", arcext_methods],
  ["RAR 3.70 -md4096 -s", "rar   a -cfg- -md4096 -s %options %archive %file", "-r",    rar_methods, "rar      t %archive"],  # "rar      x %archive"],
  ["ACE 2.04 -d4096 -s" , "ace32 a -cfg- -d4096  -s %options %archive %file", "-r",    ace_methods, "ace32    t %archive"],  # "ace32    x %archive"],
  ["SBC 0.970 -of"      , "sbc   c -of              %options %archive %file", "-r",    sbc_methods, "sbc      v %archive"],  # "sbc      x %archive"],
  ["7-zip 4.52"         , "7z    a                  %options %archive %file", "-r",    _7z_methods, "7z       t %archive"],  # "7z       x %archive"],
  ["UHARC 0.6 -md32768" , "uharc a -md32768         %options %archive %file", "-r",  uharc_methods, "uharc    t %archive"],  # "uharc    x %archive"],
  ["Squeez 5.2"         ,  sqc+" a -md32768 -s -m5 -au1 -fme1 -fmm1 -ppm1 -ppmm48 -ppmo10 -rgb1 %options %archive %file", "-r", sqc_methods, sqc+" t %archive"],
#  ["BSSC 0.92 -b16383"  , "bssc  e %file %archive -b16383 %options",          ""  ,   bssc_methods, "bssc.exe d %archive nul"]
            ]

# List of files/directories the benchmark is run on
$files = [
          'C:\Base\Compiler\euphoria',
#          'C:\Base\Compiler\VC',
#          'C:\Base\Doc\boost_1_32_0',
#          'C:\Base\Compiler\erl5.1.2',
          'C:\Base\Compiler\ghc-src',
          'C:\Base\Compiler\Dev-Cpp',
          'C:\Base\Compiler\Perl',
          'C:\Base\Compiler\Ruby',
          'C:\Base\Compiler\Bcc55',
          'C:\FIDO\Disk_Q\Тексты\Russian',
          'C:\Base\Compiler\msys',
          'C:\Base\Doc\Perl',
          'C:\Base\Doc\Java',
          'C:\Base\Compiler\SC7',

          'C:\Base\Doc\baza.mdb',
          'C:\Program Files\WinHugs',
          'C:\Program Files\Borland\Delphi7',
          'C:\Base\Doc\linux-2.6.14.5',
          'C:\Base\Compiler\ghc',
          'C:\--Program Files',
          'C:\Base\Compiler',
          'C:\Base\Compiler\MSVC',
          'C:\Downloads\Программирование\Haskell\darcs-get',
          'C:\Base',
          'C:\!\FreeArchiver\Tests\vyct',
          'E:\backup\!\ArcHaskell\Tests\ghc-exe',
          'E:\backup\!\ArcHaskell\Tests\ruby',
          'E:\backup\!\ArcHaskell\Tests\ghc-src',
          'E:\backup\!\ArcHaskell\Tests\hugs',
          'E:\backup\!\ArcHaskell\Tests\office.mdb',
          'E:\backup\!\ArcHaskell\Tests\both'
        ]

# File the benchmark report is written to, and its open mode ("a" - append, "w" - overwrite)
$reportfile = ["report", "a"]

# Report format: compression ratio and speed (true), or archive size and elapsed time (false)
$report_ratios = true

# Width of the column holding the names of the compression methods under test. If set to 0, it is determined automatically
$default_method_width = 0



########################################################
### Program code #######################################
########################################################

# Benchmark the archivers in `$archivers` on the files in `$files`
def main
  sleep 2  # give the user time to switch to another task
  workdir = File.join $workdir, "maketest"
  extractPath = File.join workdir, "extract"
  Dir.mkdir workdir rescue 0
  Dir.chdir workdir
  archive = (File.join workdir, "test.rk") .gsub('/','\\')
  File.delete archive rescue 0
  # Loop over all files/directories the benchmark is run on
  for file in $files
    isDir = File.stat(file).directory?
    # Total size of the data being packed and the max method name width
    bytes, max_method_width = reportFile file, $archivers
    # Loop over all archivers under test
    for archiver in $archivers
      arcname, aCmd, rOption, methods, *xCmds = archiver
      # Skip per-file packers when a whole directory with subdirectories has to be packed
      next if rOption=="" && isDir
      reportArchiver arcname
      # Loop over all compression methods under test for this archiver
      for method in methods
        if method=="--" then report ""; next; end
        # Build the pack/test/extract commands from the templates
        commands = ([aCmd]+xCmds).map {|cmd| cmd.gsub( "%options", method+(isDir ? " "+rOption : "")).
                                                 gsub( "%archive", archive).
                                                 gsub( "%file",    isDir ? "" : file)}
        Dir.chdir file  if isDir
        cache file      if bytes < $ramsize*3/4
        # Run the commands and measure the execution time of each one
        times = commands.map {|cmd| cacheCmd cmd, archive
                                    time = tSystem cmd
                                    prepareExtractDir extractPath  # switch to the extraction directory and clean it
                                    time
                             }
        reportResults method, bytes, archive, times, max_method_width
        File.delete archive
      end
    end
  end
end

# Run a command and return its elapsed time
def tSystem cmd
  puts
  puts cmd.gsub(/cmd \/c start \/w /,'')
  sleep 1
  t0 = Time.now
  system cmd
  return Time.now - t0
end

# Recursively walk all files in the given directory and its subdirectories
def recurse filename, &action
  if File.stat(filename).directory?
    for f in Dir[filename+'/*']
      if f!='.' && f!='..'
        recurse f, &action
      end
    end
  else
    action.call filename
  end
end

# Total number of files in a directory and their total size (for a file returns (1, filesize))
def filesAndBytes filename
  totalFiles = totalBytes = 0
  recurse filename do |f|
    totalFiles += 1
    totalBytes += File.size(f)
  end
  return totalFiles, totalBytes
end

# Read (cache) the given file, or all files in the directory and its subdirectories
def cache filename
  puts "Caching files..."
  recurse filename do |f|
    File.open f do |h|
      h.binmode
      1 while h.read(64*1024)
    end
  end
  GC.start
end

# Read (cache) the executable file of the command
def cacheCmd cmd, archive
  system ((cmd.split ' ')[0] + " -unknown-option <nul >nul")
  cache archive  if FileTest.exists? (archive)
end

# Prepare the directory to be used for extracting files
def prepareExtractDir dirname
  exit unless dirname =~ /temp/     # fool proof
  Dir.mkdir dirname rescue 0
  removeDirRecursively dirname
  Dir.mkdir dirname rescue 0
  Dir.chdir dirname
end

# Remove a directory recursively
def removeDirRecursively dirname
  if File.stat(dirname).directory?
    for f in Dir.new(dirname)
      if f!='.' && f!='..'
        removeDirRecursively (dirname+'/'+f)
      end
    end
    Dir.delete dirname rescue 0
  else
    File.delete dirname
  end
end


########################################################
### Benchmark report generation subroutines ############
########################################################

# File the benchmark report is written to
### Benchmark report generation subroutines ############
########################################################

# File the benchmark report is written to
$outfile = File.open(*$reportfile)
$outfile.sync = true

# Write line `s` to the report
def report s
  $outfile.puts s
end

# Write the benchmark header for file/directory `file` to the report and return its size
def reportFile filename, archivers
  # Compute the maximum width among the compression method names
  max_method_width = archivers .map { |x| x[3]} .flatten .map {|s| s.length} .max

  report ""  # Add a blank line before a new file
  files, bytes = filesAndBytes filename
  if files==1
    report (sprintf "%s (%d bytes)", filename, bytes)
  else
    report (sprintf "%s (%d files, %d bytes)", filename, files, bytes)
  end
  return bytes.to_f, $default_method_width>0? $default_method_width : max_method_width
end

# Write the benchmark header for a single archiver to the report
def reportArchiver archiverName
  report archiverName
end

# Write the benchmark results for method `method` to the report
def reportResults method, bytes, archive, times, max_method_width
  cbytes = File.size(archive).to_f  # Size of the compressed data
  ratio  = bytes/cbytes             # Compression ratio
  formatTimes  = times.map {|time| sprintf "%6.3f", time}            # Pack/test/extract time
  formatSpeeds = times.map {|time| sprintf "%6.3f", bytes/time/1e6}  # Pack/test/extract speed (in MB/sec)
  if $report_ratios
    # Normal report format - with compression ratio and speed
    report (sprintf " %-*s %6.3f %s", max_method_width, method, ratio, formatSpeeds.join(" "))
  else
    # Alternative report format - with archive size and elapsed time
    report (sprintf " %-*s %9d %s", max_method_width, method, cbytes, formatTimes.join(" "))
  end
end


########################################################
### Main function call #################################
########################################################

main
