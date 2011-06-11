require 'sinatra'
require 'json'
require 'activesupport'
require 'haml'

def json(x)
  JSON.pretty_generate(x)
end

Puzzles = begin
  d = {} 
  IO.foreach(File.join(File.dirname(__FILE__), '..', 'puzzles')) do |line|
    id, data = line.split(':')
    d[id] = JSON.parse(data)
  end
  class <<d
    def random_puzzle
      key = keys.random_element
      [key, *self[key]]
    end

    def find(id)
      value = self.fetch(id)
      [id, *value]
    end
  end
  d
end

get "/puzzles/next" do
  # TODO, random samples like this: culled = children.sample(10 + rand(10))
  json(Puzzles.random_puzzle)
end

get "/puzzles/:id" do
  json(Puzzles.find(params[:id]))
end

get "/" do
  haml :index
end
