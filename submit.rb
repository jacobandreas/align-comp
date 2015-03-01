#!/usr/bin/ruby

javac = "/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Commands/javac"

if ARGV.length != 1
  puts "ERROR: describe me"
  exit
end

desc = ARGV[0]

timestamp=`date +%Y-%m-%d_%H-%M-%S`.strip
work_dir="/work2/jda/instructions/work/#{desc}_#{timestamp}"

ssh_command_1 = <<-eof
  mkdir "#{work_dir}";
eof

ssh_command_2 = <<-eof
  cd "#{work_dir}";
  qsub ../../pbs.sh;
eof

puts "building assembly"
`sbt assembly`
puts "archiving"
`tar czfh src.tar.gz src`
puts "preparing run environment"
`ssh zen.millennium.berkeley.edu "#{ssh_command_1}"`
puts "uploading source"
`scp src.tar.gz zen.millennium.berkeley.edu:#{work_dir}`
puts "uploading assembly"
`scp target/scala-2.11/instructions-assembly-0.1-SNAPSHOT.jar zen.millennium.berkeley.edu:#{work_dir}/assembly.jar`
puts "starting job"
`ssh zen.millennium.berkeley.edu "#{ssh_command_2}"`
puts "done"
