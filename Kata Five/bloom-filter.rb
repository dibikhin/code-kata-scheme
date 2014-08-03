class BloomFilter
	def refresh_vector(dictionary_file_name, vector_file_name)
		dictionary = load_dictionary(dictionary_file_name)
		vector = compute_vector(dictionary)
		save_vector(vector)
	end

	def check_word(word, vector_file_name)
		vector = load_vector(vector_file_name)
		word_hash = compute_hash(word)
		is_in_vector(word_hash, vector)
	end
end