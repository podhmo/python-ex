PACKAGE_DIR = "python-ex"
require 'fileutils'

# desc "download_depends"
# task :download_depends do
#   sh <<SH
# git clone git@github.com:podhmo/el-util-macro.git
# SH
# ened

desc "package"
task :package do
  FileUtils.rm_r PACKAGE_DIR if File.exist?(PACKAGE_DIR)
  sh <<SH
mkdir -p #{PACKAGE_DIR}
cp `find . -name "*.el"` #{PACKAGE_DIR}
cat init.el | grep -v add-to-list > #{File.join(PACKAGE_DIR,'init.el')}
SH
end

desc "help"
task :help do
puts <<HELP
## extract only el files
rake package  #generate `python-ex' directory
HELP
end
task :default  => []
