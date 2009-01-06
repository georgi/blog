require 'shinmun'

use Rack::Session::Cookie
use Rack::Reloader unless ENV['RACK_ENV'] == 'production'

run Shinmun::Blog.new(File.dirname(__FILE__))
