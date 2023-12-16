module simplified_sha256 #(parameter integer NUM_OF_WORDS = 40)(
 input logic  clk, rst_n, start,
 input logic  [15:0] input_addr, hash_addr,
 output logic done, memory_clk, memory_we,
 output logic [15:0] memory_addr,
 output logic [31:0] memory_write_data,
 input logic [31:0] memory_read_data);

// FSM state variables 
enum logic [2:0] {IDLE, WAIT, BLOCK, PAD, COMPUTE, WRITE} /*state,*/next_state;

parameter integer SIZE = NUM_OF_WORDS*32; 

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[16];
logic [31:0] message[16];
logic [31:0] wt;
//logic [31:0] S0,S1;
logic [31:0] hash[8]; //0, hash1, hash2, hash3, hash4, hash5, hash6, hash7;
logic [31:0] A, B, C, D, E, F, G, H;
logic [31:0] a,b,c,d,e,f,g,h;
logic [6:0] i;
logic [NUM_OF_WORDS/16+1:0] count;
logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic        enable_write;
logic [15:0] present_addr;
logic [31:0] present_write_data;
//logic [31:0] data_read[8];
logic [31:0] paddedBits;
//logic [ 7:0] tstep;




// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign memory_clk = clk;
assign memory_addr = present_addr + offset;
assign memory_we = enable_write;
assign memory_write_data = present_write_data;


assign num_blocks = determine_num_blocks(NUM_OF_WORDS); 
//assign tstep = (i - 1);

// Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
// Function to determine number of blocks in memory to fetch
function automatic logic [15:0] determine_num_blocks(input logic [31:0] size);

  // Student to add function implementation

  // bit_length = size * 32 + 1 + 64, each block is 512 bit, 
  // so num_block = bit_length / 512
  // also check if remainder exist or not/*
  logic [31:0] bit_length = size * 32 + 65;
  if (bit_length % 512)
		determine_num_blocks = bit_length / 512 + 1;
  else
		determine_num_blocks = bit_length / 512;
 
 
endfunction


// SHA256 hash round
function automatic logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
	begin
		 S0 = ror(a, 2) ^ ror(a, 13) ^ ror(a, 22);
		 maj = (a & b) ^ (a & c) ^ (b & c);
		 t2 = S0 + maj;
		 S1 = ror(e, 6) ^ ror(e, 11) ^ ror(e, 25);
		 ch = (e & f) ^ ((~e) & g);
		 t1 = h + S1 + ch + k[t] + w;
		 sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
	end
endfunction


always_comb begin

	logic [31:0] s1, s0;

		// calculate wt for specific i
			if(i < 16) begin
				wt = message[i];
			end
			else begin
				s0 = ror(w[1], 7) ^ ror(w[1], 18) ^ (w[1] >> 3);
				s1 = ror(w[14], 17) ^ ror(w[14], 19) ^ (w[14] >> 10);
				wt = w[0] + s0 + w[9] + s1;
			end
			
	// sha operations
	{a,b,c,d,e,f,g,h} = sha256_op(A, B, C, D, E, F, G, H, wt, i);

			
		
end

always_comb begin

	// determine what bits should be padded
	if(memory_addr - input_addr == NUM_OF_WORDS + 1)
		paddedBits = 32'h80000000;
	else if(count == num_blocks-1 && i == 15) 
		paddedBits = SIZE;
	else
		paddedBits = 32'h00000000;
	
end
		

// Right Rotation Example : right rotate input x by r
// Lets say input x = 1111 ffff 2222 3333 4444 6666 7777 8888
// lets say r = 4
// x >> r  will result in : 0000 1111 ffff 2222 3333 4444 6666 7777 
// x << (32-r) will result in : 8888 0000 0000 0000 0000 0000 0000 0000
// final right rotate expression is = (x >> r) | (x << (32-r));
// (0000 1111 ffff 2222 3333 4444 6666 7777) | (8888 0000 0000 0000 0000 0000 0000 0000)
// final value after right rotate = 8888 1111 ffff 2222 3333 4444 6666 7777
// Right rotation function

function logic [31:0] ror(input logic [31:0] in,
                                  input logic [7:0] s);
begin
   ror = (in >> s) | (in << (32 - s));
end
endfunction


// SHA-256 FSM 
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory
always_ff @(posedge clk, negedge rst_n) begin
  if (!rst_n) begin
		enable_write <= 0;
		present_write_data <= 0;
		present_addr <= 0;
		offset <= 0;
		next_state <= IDLE;
  end
  else begin 
	  case (next_state)
		// Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
		IDLE: begin 
			if(start) begin 
			
				// assign initial values
				hash[0] <= 32'h6a09e667; 
				hash[1] <= 32'hbb67ae85; 
				hash[2] <= 32'h3c6ef372;
				hash[3] <= 32'ha54ff53a;
				hash[4] <= 32'h510e527f;
				hash[5] <= 32'h9b05688c;
				hash[6] <= 32'h1f83d9ab;
				hash[7] <= 32'h5be0cd19;
				
				A <= 32'h6a09e667; 
				B <= 32'hbb67ae85; 
				C <= 32'h3c6ef372;
				D <= 32'ha54ff53a;
				E <= 32'h510e527f;
				F <= 32'h9b05688c;
				G <= 32'h1f83d9ab;
				H <= 32'h5be0cd19;
				
				i <= 0;
				count <= 0;
				offset <= 0;
				present_addr <= input_addr;
				next_state <= WAIT;
		   end
		end
		WAIT: begin
			next_state <= BLOCK;
			offset <= offset+1;
		end
		// SHA-256 FSM 
		// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function    
		// and write back hash value back to memory
		BLOCK: begin
		// Fetch message in 512-bit block size
		// For each of 512-bit block initiate hash value computation

			if((memory_addr - input_addr > NUM_OF_WORDS)) begin				
					// if all words from memory have been read, start padding message
					next_state <= PAD;
				end	
			else if(i < 16) begin
				
					// read word from memory
					message[i] <= memory_read_data;
					offset <= offset + 1;
					next_state <= BLOCK;
					i <= i + 1;
				
			end
			else begin
			
				// message has been filled so start computing
				A <= hash[0];
				B <= hash[1];
				C <= hash[2];
				D <= hash[3];
				E <= hash[4];
				F <= hash[5];
				G <= hash[6];
				H <= hash[7];
				i <= 0;

				next_state <= COMPUTE;
			end
			

		end
		
		
		PAD: begin
		
				if(i == 15) begin
					// finish padding and go to compute
					message[i] <= paddedBits;
						
					next_state <= COMPUTE;
					i <= 0;
					A <= hash[0];
					B <= hash[1];
					C <= hash[2];
					D <= hash[3];
					E <= hash[4];
					F <= hash[5];
					G <= hash[6];
					H <= hash[7];
				end
				else begin
				// pad message
					message[i] <= paddedBits;
					next_state <= PAD;
					i <= i + 1;
					offset <= offset + 1;
				end
			
		end
		// For each block compute hash function
		// Go back to BLOCK stage after each block hash computation is completed and if
		// there are still number of message blocks available in memory otherwise
		// move to WRITE stage
		COMPUTE: begin
		// 64 processing rounds steps for 512-bit block 
		
			//process64(hash, A, B, C, D, E, F, G, H, hash[0], hash[1],hash[2],hash[3],hash[4],hash[5],hash[6],hash[7], message);
			//data_read = {process64(hash, A, B, C, D, E, F, G, H, message)};//'{data_read[7], data_read[6], data_read[5], data_read[4], data_read[3], data_read[2], data_read[1], data_read[0]};
			//hash <= '{data_read[7], data_read[6], data_read[5], data_read[4], data_read[3], data_read[2], data_read[1], data_read[0]};
			
			
			if(i == 64) begin //finished processing block
				count <= count + 1;
				
				//hash values are available now so assign them
				hash[0] <= hash[0] + A;
				hash[1] <= hash[1] + B;
				hash[2] <=	hash[2] + C;
				hash[3] <=	hash[3] + D;
				hash[4] <= hash[4] + E;
				hash[5] <= hash[5] + F;
				hash[6] <= hash[6] + G;
				hash[7] <= hash[7] + H;
					
				if(count == num_blocks-1) begin			// write if all message blocks have been computed		
					offset <= 0;
					enable_write <= 1;

					present_addr <= hash_addr-1;
					next_state <= WRITE;
				end
				else begin // otherwise go get another block
					offset <= offset-1;
					i <= 0;
					next_state <= WAIT;
				end
					
			end
			else begin
				
				//do one round of processing
				A <= a;
				B <= b;
				C <= c;
				D <= d;
				E <= e;
				F <= f;
				G <= g;
				H <= h;
				
				//assign w and shift it down
				for(int n = 0; n<15; n++) begin
					w[n] <= w[n+1];
				end
				w[15] <= wt;
				i <= i+1;
				next_state<= COMPUTE;
			end
			
			
					
		end

		// h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
		// h0 to h7 after compute stage has final computed hash value
		// write back these h0 to h7 to memory starting from output_addr
		WRITE: begin
			
				if(offset == 8) begin // completed write so go to IDLE
					enable_write <= 0;
					next_state <= IDLE;
				end
				else begin
					// write one memory address then go to next address
					offset <= offset+1;
					
					present_write_data <= hash[offset];
					
					
					next_state <= WRITE;
				end
		end
      endcase
	end
end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (next_state == IDLE);

endmodule
