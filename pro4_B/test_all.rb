#!/usr/bin/env ruby

require "open3"

class TestRunner
    @@SEARCH_ROOT = File.dirname(__FILE__)
    @@TESTFILE_NAME = /^gotest_[a-zA-Z0-9_\-]*\.rb$/

    def initialize(root_nodes)
        @num_run = 0
        @successes = []
        @failures = []
        @root_nodes = root_nodes.map{|f| File.join(@@SEARCH_ROOT, f)}
    end

    def run
        @root_nodes.each{|root_node| run_test_dfs(root_node)}

        puts "****************************************"
        puts "* Successfully ran tests! Test report: *"
        puts "****************************************"

        puts "Total tests run: #{@num_run}"
        puts "\tPassed: #{@successes.size}"
        puts "\tFailed: #{@failures.size}\n"
    end

    def run_test_dfs(current)
        # If the current directory doesn't exist, just return
        unless File.exists?(current) && File.directory?(current)
            return
        end

        # If the test file is here, run it, count it, and end recursion

        matches = Dir.entries(current).select{|e| e =~ @@TESTFILE_NAME}

        if matches.length >= 1 && File.exists?(File.join(current, matches[0]))
            matches.each{|e|
                stdout, stderr, status = Open3.capture3("ruby #{File.join(current, e)}")
                puts stdout
                puts stderr

                if (status == 0)
                    @successes.push e
                else
                    @failures.push e
                end

                @num_run += 1

            }
        else
            # If the file isn't here
            Dir.entries(current).select{|entry|
                !(entry == '.' || entry == '..')
            }.map{|entry|
                File.join(current, entry)
            }.select{|entry|
                File.directory?(entry)
            }.each{|entry|
                run_test_dfs(entry)
            }
        end
    end
end

TestRunner.new(["."]).run
