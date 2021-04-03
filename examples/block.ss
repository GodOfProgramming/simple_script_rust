let foo;
{
  foo = 1;
  let bar = "not bar";
  bar = "bar";
  print bar;
  {
    let bar = true;
    print bar;
  }
  end bar;
}

end true;
