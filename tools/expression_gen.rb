#!/usr/bin/env ruby
require 'erb'
require 'rails'
require 'ostruct'

contents = nil

File.open('config/expr.json') do |f|
  contents = f.read
end

expressions = JSON.parse(contents)

enums = expressions.map do |config|
  OpenStruct.new(
    name: config['name'],
    members: config['members'].map do |m| 
      OpenStruct.new(name: m['name'], type: m['type'])
    end
  )
end

renderer = nil

File.open("#{__dir__}/expr.erb") do |file|
  renderer = ERB.new(file.read, nil, '-')
end

contents = renderer.result

contents = 
`rustfmt --emit stdout <<EOF
#{contents}
EOF`

puts contents

File.open('src/expr.rs', 'w') do |f|
  f.write(contents)
end

