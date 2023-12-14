module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] header_addr, hash_out_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] memory_addr,
                    output logic [31:0] memory_write_data,
                     input logic [31:0] memory_read_data);

parameter num_nonces = 16;
enum logic [2:0] {IDLE, WAIT, BLOCK, PIPE, COMPUTE, WRITE} next_state;
//logic [31:0] hash_out[num_nonces];

parameter int k[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

// Student to add rest of the code here



// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[16];
logic [31:0] w1[16];
logic [31:0] message[16];
logic [31:0] message1[16];
logic [31:0] wt;
logic [31:0] wt1;
logic [31:0] fhash[8]; 
logic [31:0] hash[16][8];
logic [31:0] A, B, C, D, E, F, G, H;
logic [31:0] a,b,c,d,e,f,g,h;
logic [31:0] A1, B1, C1, D1, E1, F1, G1, H1;
logic [31:0] a1,b1,c1,d1,e1,f1,g1,h1;
logic [6:0] i;
logic [6:0] count;
logic [6:0] next_count;
logic [15:0] offset; // in word address

logic        enable_write;
logic [15:0] present_addr;
logic [31:0] present_write_data;
//logic [31:0] paddedBits;


assign next_count = count + 1;


// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign mem_clk = clk;
assign memory_addr = present_addr + offset;
assign mem_we = enable_write;
assign memory_write_data = present_write_data;


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
	
		
		

				
				if(i < 16) begin
					wt = message[i];
				end
				else begin
					s0 = ror(w[1], 7) ^ ror(w[1], 18) ^ (w[1] >> 3);
					s1 = ror(w[14], 17) ^ ror(w[14], 19) ^ (w[14] >> 10);
					wt = w[0] + s0 + w[9] + s1;
				end
				

			
			a = A;
			b = B;
			c = C;
			d = D;
			e = E;
			f = F;
			g = G;
			h = H;
		

	
		{a,b,c,d,e,f,g,h} = sha256_op(a, b, c, d, e, f, g, h, wt, i);	
end

always_comb begin
	
		logic [31:0] s1, s0;
	
		
		

				
				if(i < 16) begin
					wt1 = message1[i];
				end
				else begin
					s0 = ror(w1[1], 7) ^ ror(w1[1], 18) ^ (w1[1] >> 3);
					s1 = ror(w1[14], 17) ^ ror(w1[14], 19) ^ (w1[14] >> 10);
					wt1 = w1[0] + s0 + w1[9] + s1;
				end
				

			
			a1 = A1;
			b1 = B1;
			c1 = C1;
			d1 = D1;
			e1 = E1;
			f1 = F1;
			g1 = G1;
			h1 = H1;
		

	
		{a1,b1,c1,d1,e1,f1,g1,h1} = sha256_op(a1, b1, c1, d1, e1, f1, g1, h1, wt1, i);	
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
always_ff @(posedge clk, negedge reset_n) begin
  if (!reset_n) begin
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
				fhash[0] <= 32'h6a09e667; 
				fhash[1] <= 32'hbb67ae85; 
				fhash[2] <= 32'h3c6ef372;
				fhash[3] <= 32'ha54ff53a;
				fhash[4] <= 32'h510e527f;
				fhash[5] <= 32'h9b05688c;
				fhash[6] <= 32'h1f83d9ab;
				fhash[7] <= 32'h5be0cd19;
				
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
				present_addr <= header_addr;
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
			if(count != 0 && i == 3) begin
				message[3] = 32'h00000000;
				message[4] = 32'h80000000;
				for(int i = 5; i < 15; i++) begin
					message[i] = 32'h00000000;
				end
				message[15] = 32'd640;
				A <= fhash[0];
				B <= fhash[1];
				C <= fhash[2];
				D <= fhash[3];
				E <= fhash[4];
				F <= fhash[5];
				G <= fhash[6];
				H <= fhash[7];
				i <= 0;
		//		offset <= offset - 1;

				next_state <= COMPUTE;
				
			end
			else if(i < 16) begin
				
				 // Get message of fixed part
					message[i] <= memory_read_data;
					offset <= offset + 1;
					next_state <= BLOCK;
					i <= i + 1;
				
			end
			else begin
			
			
				
				A <= fhash[0];
				B <= fhash[1];
				C <= fhash[2];
				D <= fhash[3];
				E <= fhash[4];
				F <= fhash[5];
				G <= fhash[6];
				H <= fhash[7];
				i <= 0;
		//		offset <= offset - 1;

				next_state <= COMPUTE;
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
				
				
					
				if(count != 0) begin
					// hash of the first nonce
					hash[0][0] <= 32'h6a09e667;
					hash[0][1] <= 32'hbb67ae85; 
					hash[0][2] <=	32'h3c6ef372;
					hash[0][3] <=	32'ha54ff53a;
					hash[0][4] <= 32'h510e527f;
					hash[0][5] <= 32'h9b05688c;
					hash[0][6] <= 32'h1f83d9ab;
					hash[0][7] <= 32'h5be0cd19;
					
					
					// Use hash of first nonce to initialize values for computing hash of first 8 word hash
					A1 <= 32'h6a09e667; 
					B1 <= 32'hbb67ae85; 
					C1 <= 32'h3c6ef372;
					D1 <= 32'ha54ff53a;
					E1 <= 32'h510e527f;
					F1 <= 32'h9b05688c;
					G1 <= 32'h1f83d9ab;
					H1 <= 32'h5be0cd19;
		
					message1[0] <= fhash[0] + A;
					message1[1] <= fhash[1] + B;
					message1[2] <=	fhash[2] + C;
					message1[3] <=	fhash[3] + D;
					message1[4] <= fhash[4] + E;
					message1[5] <= fhash[5] + F;
					message1[6] <= fhash[6] + G;
					message1[7] <= fhash[7] + H;
					
					message1[8] <= 32'h80000000;
					for(int i = 9; i<15; i++) begin
						message1[i] <= 32'h00000000;
					end
					message1[15] <= 32'd256;
					
					offset <= 0;
					i <= 0;
					//enable_write <= 1;
					
					//initialize values to compute hash of second nonce
					message[3] <= message[3]+1;
					A <= fhash[0];
					B <= fhash[1];
					C <= fhash[2];
					D <= fhash[3];
					E <= fhash[4];
					F <= fhash[5];
					G <= fhash[6];
					H <= fhash[7];
					
					count <= 0;
					//present_addr <= hash_addr-1;
					next_state <= PIPE;
				end
				else begin
					// compute hash of fixed part
					fhash[0] <= fhash[0] + A;
					fhash[1] <= fhash[1] + B;
					fhash[2] <=	fhash[2] + C;
					fhash[3] <=	fhash[3] + D;
					fhash[4] <= fhash[4] + E;
					fhash[5] <= fhash[5] + F;
					fhash[6] <= fhash[6] + G;
					fhash[7] <= fhash[7] + H;
					offset <= offset-1;
					i <= 0;
					count <= count+1;
					next_state <= WAIT;
				end
					
			end
			else begin
			
				A <= a;
				B <= b;
				C <= c;
				D <= d;
				E <= e;
				F <= f;
				G <= g;
				H <= h;
				for(int n = 0; n<15; n++) begin
					w[n] <= w[n+1];
				end
				w[15] <= wt;
				i <= i+1;
				next_state<= COMPUTE;
			end
			
			
					
		end
		PIPE: begin
		
			if(i == 64) begin
				count <= count + 1;
				
					
				if(count != 15) begin
					// final hash(1b) of nonce #count 
					hash[count][0] <= 32'h6a09e667 + A1;
					hash[count][1] <= 32'hbb67ae85 + B1;
					hash[count][2] <=	32'h3c6ef372 + C1;
					hash[count][3] <=	32'ha54ff53a + D1;
					hash[count][4] <= 32'h510e527f + E1;
					hash[count][5] <= 32'h9b05688c + F1;
					hash[count][6] <= 32'h1f83d9ab + G1;
					hash[count][7] <= 32'h5be0cd19 + H1;
					
					// Use hash(2a) to initialize values for computing hash of 8 word hash 
					
					A1 <= 32'h6a09e667; 
					B1 <= 32'hbb67ae85; 
					C1 <= 32'h3c6ef372;
					D1 <= 32'ha54ff53a;
					E1 <= 32'h510e527f;
					F1 <= 32'h9b05688c;
					G1 <= 32'h1f83d9ab;
					H1 <= 32'h5be0cd19;
					
					message1[0] <= fhash[0] + A;
					message1[1] <= fhash[1] + B;
					message1[2] <=	fhash[2] + C;
					message1[3] <=	fhash[3] + D;
					message1[4] <= fhash[4] + E;
					message1[5] <= fhash[5] + F;
					message1[6] <= fhash[6] + G;
					message1[7] <= fhash[7] + H;
					
					message1[8] <= 32'h80000000;
					for(int i = 9; i<15; i++) begin
						message1[i] <= 32'h00000000;
					end
					message1[15] <= 32'd256;
					
					offset <= 0;
					i <= 0;
					//enable_write <= 1;
					
					//initialize values to compute hash of second nonce
					message[3] <= message[3]+1;
					A <= fhash[0];
					B <= fhash[1];
					C <= fhash[2];
					D <= fhash[3];
					E <= fhash[4];
					F <= fhash[5];
					G <= fhash[6];
					H <= fhash[7];


					
					next_state <= PIPE;
				end
				else begin
					hash[count][0] <= 32'h6a09e667 + A1;
					hash[count][1] <= 32'hbb67ae85 + B1;
					hash[count][2] <=	32'h3c6ef372 + C1;
					hash[count][3] <=	32'ha54ff53a + D1;
					hash[count][4] <= 32'h510e527f + E1;
					hash[count][5] <= 32'h9b05688c + F1;
					hash[count][6] <= 32'h1f83d9ab + G1;
					hash[count][7] <= 32'h5be0cd19 + H1;
					offset <= 0;
					i <= 0;
					enable_write <= 1;
					present_addr <= hash_out_addr-1;
					next_state <= WRITE;
				end
			end 
			else begin
				// Compute hash from new nonce
				A <= a;
				B <= b;
				C <= c;
				D <= d;
				E <= e;
				F <= f;
				G <= g;
				H <= h;
				for(int n = 0; n<15; n++) begin
					w[n] <= w[n+1];
				end
				w[15] <= wt;
				
				// Compute hash from 8 word hash
				A1 <= a1;
				B1 <= b1;
				C1 <= c1;
				D1 <= d1;
				E1 <= e1;
				F1 <= f1;
				G1 <= g1;
				H1 <= h1;
				for(int n = 0; n<15; n++) begin
					w1[n] <= w1[n+1];
				end
				w1[15] <= wt1;
				i <= i+1;
				next_state<= PIPE;
			end
			
		end

		// h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
		// h0 to h7 after compute stage has final computed hash value
		// write back these h0 to h7 to memory starting from output_addr
		WRITE: begin
		
				if(offset == 16) begin
					enable_write <= 0;
					next_state <= IDLE;
				end
				else begin
					
					offset <= offset+1;
					
					present_write_data <= hash[offset][0];
					
					
					next_state <= WRITE;
				end
		end
      endcase
	end
end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (next_state == IDLE);






endmodule
