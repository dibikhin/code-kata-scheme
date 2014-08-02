class BloomFilter
	def initialize(file_name)
		@bit_vector = load_filter(file_name)
	end
	
	def self.load_filter(file_name)
	end

	def compute_bit_vector(word_list_file_name)
	end

	def refresh_bit_vector(new_bit_vector)
		@bit_vector = new_bit_vector
	end

	def check_word(word)
	end
end