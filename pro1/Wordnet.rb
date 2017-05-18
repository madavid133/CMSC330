class WordNet
     
    require 'set'

   def initialize (syn_file, hyper_file)
        
        @synset = Hash.new
        @hypernyms = Hash.new
        index1 = Array.new
        index2 = Array.new
        a = 0
        b = 0
        
        IO.foreach(syn_file) { |x|
            x = x.chomp
            #puts x.include?("\n")
            if !(x =~ /^id: \d+ synset: [^\s,]+(,[^\s,]+)*$/) then
                index1.push(x)
                a = a + 1
            else
                synset_ID = x.split(' ')[1].to_i
                if !(@synset.has_key? (synset_ID)) then
                    @synset[synset_ID] = x.split(' ')[3].split(',')
                elsif (@synset.has_key? (synset_ID))
                    @synset[synset_ID] = @synset.fetch(synset_ID) + x.split(' ')[3].split(',')
                end
            end 
        }
        
        if a != 0 
            print ("invalid synsets\n")
            index1.each { |x|
                puts x
                }   
            exit
        end
        
        IO.foreach(hyper_file) { |x|
            x = x.chomp
            #puts x.include?("\n")
            if !(x =~ /^from: \d+ to: \d+(,\d+)*$/) then
                index2.push(x)
                b = b + 1
            else
                hyper_ID = x.split(' ')[1].to_i
                hypers = x.split(' ')[3].split(',')
                (0..hypers.size-1).each do |i| hypers[i] = hypers[i].to_i end
                if !(@hypernyms.has_key? (hyper_ID)) then
                    @hypernyms[hyper_ID] = hypers
                elsif (@hypernyms.has_key? (hyper_ID))
                    @hypernyms[hyper_ID] = @hypernyms.fetch(hyper_ID) + hypers
                end 
            end       

        }

        if b != 0 
            print ("invalid hypernyms\n")
            index2.each { |x|
                puts x 
            }
            exit
        end
    end

    def isnoun(input)
        #puts input
        input.delete_if do |x| 
            #puts x
            flag = false
            @synset.values.each do |y|
                if y.include?(x)
                    flag = true
                    break
                end
            end
            flag
        end
        #puts input
        return input.length == 0
   
        # index = 0
        # set = Set.new
        # for i in 0..(input.length - 1)
        #     @synset.values.each do |x|
        #     if x.include?(input[i])
        #         if not set.include?(input[i])
        #             index = index + 1
        #             set.add(input[i])
        #         end
        #     end
        #     end
        # end

        # if index == input.length
        #     return true
        # else
        #     return false
        # end
    end

    def nouns
        index = 0
        @synset.values.each do |x|
            index = index + x.length
        end
        return index
    end

    def edges
        index = 0
        @hypernyms.each_value do |x|
            index = index + x.length
        end
        return index
    end

    # def check(v,w)
    #     # for i in v
    #     #     if @synset[i] == nil
    #     #         return -1
    #     #     end
    #     # end
    #     # for j in w
    #     #     if @synset[j] == nil
    #     #     return -1
    #     #     end
    #     # end
    #     hasV = false
    #     hasW = false
    #     @synset.each do |key, value|
    #         for

    #         if key == v or value.include?(v)
    #             hasV = true
    #         end
    #         if key == w or value.include?(w)
    #             hasW = true
    #         end
    #     end
    #     puts hasW
    #     puts hasV
    #     if hasV and hasW
    #         return 1
    #     else
    #         return -1
    #     end

    # end


    def length(v, w)
        # notFound = check(v,w)
        # if notFound == -1
        #     return -1
        # end 
        minLen = +1.0/0.0
        lens1 = {}  
        lens2 = {}
        for i in v
            lst = BFS(i)
            #puts lst
            for x in lst
                if not lens1.has_key?(x[0]) or lens1[x[0]] > x[1]
                    lens1[x[0]] = x[1]
                end
            end
        end
        for j in w
            lst = BFS(j)
            #puts lst
            for x in lst
                if not lens2.has_key?(x[0]) or lens2[x[0]] > x[1]
                    lens2[x[0]] = x[1]
                end
            end
        end

        lens1.each do |key, value|
            if lens2.has_key?(key)
                len = value + lens2[key]
                if len < minLen
                    minLen = len
                end

            end
        end

        if minLen == +1.0/0.0
            return -1
        else
            return minLen
        end

    end

    def ancestor(v,w)
        # notFound = check(v,w)
        # if notFound == -1
        #     return [-1]
        # end 
        minLen = +1.0/0.0
        ancestors = [-1]
        lens1 = {}  
        lens2 = {}
        for i in v
            lst = BFS(i)
            #puts lst
            for x in lst
                if not lens1.has_key?(x[0]) or lens1[x[0]] > x[1]
                    lens1[x[0]] = x[1]
                end
            end
        end
        for j in w
            lst = BFS(j)
            #puts lst
            for x in lst
                if not lens2.has_key?(x[0]) or lens2[x[0]] > x[1]
                    lens2[x[0]] = x[1]
                end
            end
        end

        #puts lens1
        #puts lens2
        lens1.each do |key, value|
            if lens2.has_key?(key)
                len = value + lens2[key]
                if len < minLen
                    minLen = len
                    ancestors = []
                    ancestors.push(key)
                elsif len == minLen
                    ancestors.push(key)
                end

            end
        end
        return ancestors
    end

    def root(v, w)
        v_list = []
        w_list = []
        @synset.each do |key, value|
            if value.include?(v)
                v_list.push(key)
            end
            if value.include?(w)
                w_list.push(key)
            end
        end
        ancestors = ancestor(v_list, w_list)
        if ancestors.length == 1 && ancestors[0] == -1
            return [-1]
        end
        result = []
        for a in ancestors
            n_arr = @synset[a]
            for n in n_arr
                result.push(n)
            end
        end
        return result
    end

    def BFS(v)
        ret = []
        queue = Queue.new
        set = Set.new
        #n = Array.new
        if not @synset.has_key?(v)
            return []
        end
        queue.push([v, 0])
        set.add(v)
        while !queue.empty?()
            t = queue.pop()
            ret.push(t)
            neighbors = @hypernyms[t[0]]
            if neighbors != nil
                for n in neighbors
                    if not set.include?(n)
                        queue.push([n, t[1]+1])
                        set.add(n)
                    end
                end
            end
        end
        return ret
    end
   
        
    def outcast (nouns)
        a = Hash.new()
        b = Hash.new(0)
        ret = nouns.size-1
        index = Array.new()
        outcasts = Array.new()

        for i in 0..ret
            @synset.each{ |key, value|
            if (value.include?(nouns[i])) then   
                if a[nouns[i]].nil? then
                    a[nouns[i]] = [key]
                elsif !a[nouns[i]].include?(key)
                    a[nouns[i]].push(key)
                end    
            end
            }
        end   
        ret2 = a.size-1
        for i in 0..ret2
            for j in 0..ret
                dist = length(a.values[i], a[nouns[j]])
                b[a.keys[i]] = b[a.keys[i]] + dist * dist
            end
        end

        b.each{|k,v| 
            if v == b.values.max then   
                index.push(k)
            end
        } 
        for i in 0..ret
            if index.include?(nouns[i]) then
                outcasts.push(nouns[i])
            end
        end
        return outcasts
    end




end

#If the result is an array, then the array's contents will be printed in a sorted and space-delimited string. 
#Otherwise, the result is printed as-is
def print_res(res)
    if (res.instance_of? Array) then 
        str = ""
        res.sort.each {|elem| str += elem.to_s + " "}
        puts str.chomp
    else 
        puts res
    end
end 

#Checks that the user has provided an appropriate amount of arguments
if (ARGV.length < 3 || ARGV.length > 5) then
  fail "usage: wordnet.rb <synsets file> <hypersets file> <command> <input file>"
end

synsets_file = ARGV[0]
hypernyms_file = ARGV[1]
command = ARGV[2]
input_file = ARGV[3]

wordnet = WordNet.new(synsets_file, hypernyms_file)

#Refers to number of lines in input file
commands_with_0_input = %w(edges nouns)
commands_with_1_input = %w(isnoun outcast)
commands_with_2_input = %w(length ancestor)

#Executes the program according to the provided mode
case command
when *commands_with_0_input
	puts wordnet.send(command)
when *commands_with_1_input 
	file = File.open(input_file)
	nouns = file.gets.split(/\s/)
	file.close    
    print_res(wordnet.send(command, nouns))
when *commands_with_2_input 
	file = File.open(input_file)   
	v = file.gets.split(/\s/).map(&:to_i)
	w = file.gets.split(/\s/).map(&:to_i)
	file.close
    print_res(wordnet.send(command, v, w))
when "root"
	file = File.open(input_file)
	v = file.gets.strip
	w = file.gets.strip
	file.close
    print_res(wordnet.send(command, v, w))
else
  fail "Invalid command"
end
