require 'shinmun'

`git init`
`git remote add origin git@github.com:georgi/blog.git`
`git pull origin master`

use Rack::Session::Cookie
use Rack::Reloader unless ENV['RACK_ENV'] == 'production'

run Shinmun::Blog.new(File.dirname(__FILE__))
