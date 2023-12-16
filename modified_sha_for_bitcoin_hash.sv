module sha_256 (
 input logic  clk, reset_n, start, first_or_sec,
 input logic[31:0] pre_hash[8],
 input logic  [31:0] message[19],
 input logic [31:0] nounce,
 output logic done,
 output logic [31:0] hash_val[8]);

// FSM state variables 
enum logic [2:0] {IDLE, COMPUTE} /*state,*/next_state;

//parameter integer SIZE = NUM_OF_WORDS*32; 

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[16];
logic [31:0] wt;
logic [31:0] A, B, C, D, E, F, G, H;
logic [6:0] i,j;


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


function logic [31:0] wtnew; // the opt word expansion
	logic [31:0] s0, s1;
	begin
		s0 = ror(w[1],7) ^ ror(w[1],18) ^ (w[1] >> 3);
		s1 = ror(w[14],17) ^ ror(w[14],19) ^ (w[14] >> 10);
		wtnew = w[0] + s0 + w[9] + s1;
	end
endfunction


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
		next_state <= IDLE;
  end
  else begin 
	  case (next_state)
		// Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
		IDLE: begin 
			if(start) begin 

				i <= 0;
				j <= 0;
				A <= pre_hash[0]; 
				B <= pre_hash[1]; 
				C <= pre_hash[2];
				D <= pre_hash[3];
				E <= pre_hash[4];
				F <= pre_hash[5];
				G <= pre_hash[6];
				H <= pre_hash[7];
				if (first_or_sec) begin
					w[0] <= message[16];
					w[1] <= message[17];
					w[2] <= message[18];
					w[3] <= nounce;
					w[4] <= 32'h80000000;
					w[15] <= 32'd640;
					for(int t = 5; t < 15; t++)
						w[t] <= 0;
				end
				else begin
					for(int t = 0; t < 16; t++)
						w[t] <= message[t];
				end
				next_state <= COMPUTE;
		   end
		end

		COMPUTE: begin
			// 64 processing rounds steps for 512-bit block
			if(i < 48) begin 
				{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, w[0], i);
				for (int t = 0; t < 15; t++)
					w[t] <= w[t+1];
				w[15] <= wtnew();
				i <= i + 1;
				next_state <= COMPUTE;
			end
			
			else if (i < 64) begin
				{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, H, w[i-48], i);
				i <= i + 1;
				next_state <= COMPUTE;
			end
			else if ((first_or_sec) && (j == 0)) begin
				i <= 0;
				j <= 1;
				next_state <= COMPUTE;
				w[0] <= A + pre_hash[0];
				w[1] <= B + pre_hash[1];
				w[2] <= C + pre_hash[2];
				w[3] <= D + pre_hash[3];
				w[4] <= E + pre_hash[4]; 
				w[5] <= F + pre_hash[5];
				w[6] <= G + pre_hash[6];
				w[7] <= H + pre_hash[7];
				w[8] <= 32'h80000000;
				w[15] <= 32'd256;
				for(int t = 9; t < 15; t++)
					w[t] <= 0;
				
				A <= 32'h6a09e667;
				B <= 32'hbb67ae85;
				C <= 32'h3c6ef372;
				D <= 32'ha54ff53a;
				E <= 32'h510e527f;
				F <= 32'h9b05688c;
				G <= 32'h1f83d9ab;
				H <= 32'h5be0cd19;
			end
			else begin
				hash_val[0] <= A + 32'h6a09e667;
				hash_val[1] <= B + 32'hbb67ae85;
				hash_val[2] <= C + 32'h3c6ef372;
				hash_val[3] <= D + 32'ha54ff53a;
				hash_val[4] <= E + 32'h510e527f;
				hash_val[5] <= F + 32'h9b05688c;
				hash_val[6] <= G + 32'h1f83d9ab;
				hash_val[7] <= H + 32'h5be0cd19;
				next_state <= IDLE;
			end
		end
      endcase
	end
end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (next_state == IDLE);

endmodule
