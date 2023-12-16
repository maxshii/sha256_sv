module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] header_addr, hash_out_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] memory_addr,
                    output logic [31:0] memory_write_data,
                     input logic [31:0] memory_read_data);

parameter num_nonces = 16;

enum logic [2:0] {IDLE, WAIT1, WAIT2, READ, FIRST, SECOND, WRITE} /*state,*/next_state;
logic [31:0] hash_out[num_nonces];

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
logic [31:0] message[19], first_result[8], final_result[16][8], hash[8], nounce[16];	
logic        enable_write, start1, start2, done1, done2[16];
logic [15:0] present_addr;
logic [31:0] present_write_data;
logic [15:0] offset; // in word address
logic second = 1;
logic first = 0;

assign mem_clk = clk; 
assign memory_addr = present_addr + offset;
assign mem_we = enable_write;
assign memory_write_data = present_write_data;



sha_256 block1( // deals the first block
	.clk(clk), 
	.reset_n(reset_n), 
	.start(start1), 
	.first_or_sec(first), 
	.pre_hash(hash), 
	.message(message), 
	.nounce(nounce[0]),
	.done(done1), 
	.hash_val(first_result));

genvar i;
generate
	for(i = 0; i < 16; i++) begin: block2
		sha_256 block2(	// deals the second block
			.clk(clk), 
			.reset_n(reset_n), 
			.start(start2), 
			.first_or_sec(second), 
			.pre_hash(first_result), 
			.message(message), 
			.nounce(nounce[i]),
			.done(done2[i]), 
			.hash_val(final_result[i]));
	end : block2
endgenerate

always_ff @(posedge clk, negedge reset_n) begin
  if (!reset_n) begin
		enable_write <= 0;
		next_state <= IDLE;
  end
  else begin 
	  case (next_state)
		// Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
		IDLE: begin 
			if(start) begin 
				hash[0] <= 32'h6a09e667; 
				hash[1] <= 32'hbb67ae85; 
				hash[2] <= 32'h3c6ef372;
				hash[3] <= 32'ha54ff53a;
				hash[4] <= 32'h510e527f;
				hash[5] <= 32'h9b05688c;
				hash[6] <= 32'h1f83d9ab;
				hash[7] <= 32'h5be0cd19;
				offset <= 0;
				present_addr <= header_addr;
				start1 <= 0;
				start2 <= 0;
				enable_write <= 0;
				next_state <= READ;
				for(int t = 0; t < 16; t++) begin
					nounce[t] <= t;
				end
		   end
		end
		// SHA-256 FSM 
		// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function    
		// and write back hash value back to memory
		READ: begin
			if (offset == 0) begin
				offset <= offset + 1;
				next_state <= READ;
			end
			else if (offset < 20) begin
				message[offset-1] <= memory_read_data;
				offset <= offset + 1;
				next_state <= READ;
			end
			else begin
				start1 <= 1;
				next_state <= WAIT1;
				// $display("zzzzzzzzzzzzzzzzzz\n");
				// $display("%x\n",message[0]);
				// $display("%x\n",message[18]);
			end
		end
		
		WAIT1: begin
			// $display("wait1 plz");
			start1 <= 0;
			next_state <= FIRST;
		end
		
		FIRST: begin
			// $display("FIRST\n");
			if(done1) begin
				start2 <= 1;
				next_state <= WAIT2;
				// $display("here is first resu %p\n",first_result);
			end
			else
				next_state <= FIRST;
		end
		
		WAIT2: begin
			start2 <= 0;
			next_state <= SECOND;
		end
		
		SECOND: begin
			if(done2[0]) begin
				present_addr <= hash_out_addr - 1;
				offset <= 0;
				enable_write <= 1;
				next_state <= WRITE;
				// $display("here is final resu %x\n",final_result[0][0]);
			end
			else
				next_state <= SECOND;
		end
		
		WRITE: begin
			// $display("%x\n",final_result[offset][0]);
			if(offset < 16) begin
				present_write_data <= final_result[offset][0];
				offset <= offset + 1;
				next_state <= WRITE;
			end
			else begin
				next_state <= IDLE;
				enable_write <= 0;
			end
			// $display("done!!!!!!!!!!!!\n");
		end
		
	endcase
end
end

assign done = (next_state == IDLE);

endmodule