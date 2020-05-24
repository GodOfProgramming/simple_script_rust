#!/usr/bin/env ruby
require 'json'

contents = nil

File.open('config/expr.json') do |f|
  contents = f.read
end

expressions = JSON.parse(contents)

class Expr
  attr_reader :name, :members

  def initialize(name, members)
    @name = name
    @members = members.map { |m| Member.new m['name'], m['type'] }
  end
end

class Member
  attr_reader :name, :type

  def initialize(name, type)
    @name = name
    @type = type
  end
end

enums = expressions.map do |config|
  Expr.new config['name'], config['members']
end

head = "use crate::lex::{Token, Literal};\n\n"

enum_decl = "pub enum Expr {\n"

enums.each do |enum|
  enum_decl += enum.name + ' { '
  enum_decl += enum.members.map { |m| "#{m.name}: #{m.type}" }.join(',')
  enum_decl += " },\n"
end

enum_decl += "}\n\n"

impl_decl = "impl Expr {\n"

enums.each do |enum|
  impl_decl += "pub fn new_#{enum.name.downcase}("
  impl_decl += enum.members.map { |m| "#{m.name}: #{m.type}" }.join(',')
  impl_decl += ") -> Expr {\n"
  impl_decl += "Expr::#{enum.name} { "
  impl_decl += enum.members.map { |m| "#{m.name}" }.join(',')
  impl_decl += " }\n"
  impl_decl += "}\n"
end

impl_decl += "}\n\n"

contents = head + enum_decl + impl_decl

puts contents

contents = 
`rustfmt --emit stdout <<EOF
#{contents}
EOF`

File.open('src/expr.rs', 'w') do |f|
  f.write(contents)
end

