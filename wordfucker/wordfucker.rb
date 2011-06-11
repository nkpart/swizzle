require 'sinatra'
require 'json'
require 'activesupport'
require 'haml'

def json(x)
  JSON.pretty_generate(x)
end

Puzzles = begin
  d = [] 
  IO.foreach(File.join(File.dirname(__FILE__), '..', 'puzzles')) do |line|
    id, data = line.split(':')
    d << [id, *JSON.parse(data)]
  end
  d.to_a
end

get "/puzzles/next" do
  id, word, children = Puzzles.random_element
  # culled = children.sample(10 + rand(10))
  json([id, word, children])
end

get "/" do
  haml :index
end
