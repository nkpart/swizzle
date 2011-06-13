require 'sinatra'
require 'json'
require 'haml'
require 'digest/md5'
  digest = Digest::MD5.hexdigest("Hello World\n")

def json(x)
  JSON.pretty_generate(x)
end

def md5(x)
  Digest::MD5.hexdigest(x)
end

Puzzles = begin
  d = {} 
  IO.foreach(File.join(File.dirname(__FILE__), 'puzzles')) do |line|
    id, data = line.split(':')
    d[id] = JSON.parse(data)
  end

  class <<d
    def random_puzzle
      index = rand(keys.length) 
      key = keys[index]
      find(key)
    end

    def find(id)
        value = self.fetch(id)
        letters = value[0].split('').shuffle.join('')
        matches = value[1].sort_by { |x| [-x.length, x] }
        xs = matches.map { |word| [word.length, md5(word)] }
        [id, letters, xs]
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
