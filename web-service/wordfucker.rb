require 'sinatra'
require 'json'
require 'activesupport'

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
   
  json([id, word, children.sample(10 + rand(10))])
end

