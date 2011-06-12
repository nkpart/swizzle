require 'sinatra'
require 'json'
require 'activesupport'
require 'haml'

def json(x)
  JSON.pretty_generate(x)
end

Puzzles = begin
  d = {} 
  IO.foreach(File.join(File.dirname(__FILE__), 'puzzles')) do |line|
    id, data = line.split(':')
    d[id] = JSON.parse(data)
  end

  class <<d
    def random_puzzle
      key = keys.random_element
      find(key)
    end

    def find(id)
      @cache ||= {}

      @cache[id] || begin
        value = self.fetch(id)
        letters = value[0].split('').shuffle.join('')
        matches = value[1].sort_by { |x| [-x.length, x] }
        xs = matches.map { |word| [word.length,`md5 -q -s #{word}`.chomp]  }
        ret = [id, letters, xs]
        @cache[id] = ret
        ret
      end
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
