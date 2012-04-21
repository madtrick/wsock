task :clean do
  sh "rebar clean"
end

task :build => :clean do
  sh "rebar compile"
end

task :shell do
  sh "erl -pa ebin deps/*/ebin"
end

task :features do
  sh "rebar compile run-features path=test/acceptance skip_deps=true"
end

task :spec do
  sh "rebar compile && ERL_LIBS='deps/' ./espec test/spec/"
end

task :default => :build
