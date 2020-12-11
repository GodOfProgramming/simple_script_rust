#!/usr/bin/env ruby
require 'erb'
require 'rails'
require 'ostruct'

contents = nil

File.open('config/ast.json') do |f|
  contents = f.read
end

configs = JSON.parse(contents, symbolize_names: true).map do |file, cfg|
  OpenStruct.new(
    filename: file,
    name: cfg[:name],
    uses: cfg[:uses],
    enums: cfg[:enums].map do |e|
      OpenStruct.new(
        name: e[:name],
        struct_name: e[:name] + cfg[:name],
        visit_fn: "visit",
        members: e[:members].map do |m|
          OpenStruct.new(name: m[:name], type: m[:type])
        end
      )
    end
  )
end

erb_tmpl = nil

File.open("#{__dir__}/ast_tmpl.erb") do |file|
  erb_tmpl = file.read
end

for config in configs do
  puts "Generating #{config.filename}.rs"
  renderer = ERB.new(erb_tmpl, nil, '-')

  contents = renderer.result

  contents =
`rustfmt --emit stdout <<EOF
#{contents}
EOF`

  File.open("src/#{config.filename}.rs", 'w') do |f|
    f.write(contents)
  end

  puts 'done'
end
