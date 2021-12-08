module Day06
  class << self

    @@start_pool = {}
    for fish in STDIN.readline.split(',') do
      @@start_pool.merge!({ fish.to_i => 1 }) { |k, a, b| a + b}
    end

    def age pool
      new = {}
      pool.each_pair do |k, v|
        if k.zero? then
          new.merge!({ 8 => v }) { |p, a, b| a + b }
          x = 6
        else
          x = k.pred
        end
        new.merge!({x => v}) { |p, a, b| a + b }
      end
      new
    end

    def simulate time
      p = @@start_pool.merge
      for _ in (1..time) do
        p = age p
      end
      p
    end

    def solve time
      simulate(time).values.sum
    end

    puts Day06.solve 80
    puts Day06.solve 256

  end

end
