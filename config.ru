# TODO switch to haskell

use Rack::Static, 
  :urls => Dir.entries(File.dirname(__FILE__) + '/public').map {|f| '/' + f},
  :root => "public"

run lambda { |env|
  [
    200, 
    {
      'Content-Type'  => 'text/html', 
      'Cache-Control' => 'public, max-age=86400' 
    },
    File.open('public/index.html', File::RDONLY)
  ]
}
