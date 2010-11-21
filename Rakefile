PACKAGE_DIR = "python-ex"
require 'fileutils'

desc "package"
task :package do
  FileUtils.rm_r PACKAGE_DIR if File.exist?(PACKAGE_DIR)
  sh <<SH
mkdir -p #{PACKAGE_DIR}
cp `find . -name "*.el"` #{PACKAGE_DIR}
cat init.el | grep -v add-to-list > #{File.join(PACKAGE_DIR,'init.el')}
SH
end
