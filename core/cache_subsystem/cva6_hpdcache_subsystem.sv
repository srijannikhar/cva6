module cva6_hpdcache_subsystem
//  Parameters
//  {{{
#(
    parameter config_pkg::cva6_cfg_t CVA6Cfg = config_pkg::cva6_cfg_empty,
    parameter int NumPorts = 4,
    parameter int NrHwPrefetchers = 4,
    parameter type noc_req_t = logic,
    parameter type noc_resp_t = logic,
    parameter type cmo_req_t = logic,
    parameter type cmo_rsp_t = logic
)
//  }}}

//  Ports
//  {{{
(
    input logic clk_i,
    input logic rst_ni,

    //  I$
    //  {{{
        //    Cache management
    input logic icache_enable_i,  // from CSR
    input logic icache_flush_i,  // high until acknowledged
    output logic                       icache_flush_ack_o,     // send a single cycle acknowledge signal when the cache is flushed
    output logic icache_miss_o,  // we missed on a ld/st

    //  AMO interface
    input  ariane_pkg::amo_req_t                     icache_amo_req_i,    // from LSU
    output ariane_pkg::amo_resp_t                    icache_amo_resp_o,   // to LSU
    //  CMO interface
    input  cmo_req_t                                 icache_cmo_req_i,    // from CMO FU
    output cmo_rsp_t                                 icache_cmo_resp_o,   // to CMO FU
    //  Request ports
    input  ariane_pkg::dcache_req_i_t [NumPorts-1:0] icache_req_ports_i,  // from LSU
    output ariane_pkg::dcache_req_o_t [NumPorts-1:0] icache_req_ports_o,  // to LSU
    //  Write Buffer status
    output logic                                     icache_wbuffer_empty_o,
    output logic                                     icache_wbuffer_not_ni_o,

    //  Hardware memory prefetcher configuration
    input  logic [NrHwPrefetchers-1:0]       icache_hwpf_base_set_i,
    input  logic [NrHwPrefetchers-1:0][63:0] icache_hwpf_base_i,
    output logic [NrHwPrefetchers-1:0][63:0] icache_hwpf_base_o,
    input  logic [NrHwPrefetchers-1:0]       icache_hwpf_param_set_i,
    input  logic [NrHwPrefetchers-1:0][63:0] icache_hwpf_param_i,
    output logic [NrHwPrefetchers-1:0][63:0] icache_hwpf_param_o,
    input  logic [NrHwPrefetchers-1:0]       icache_hwpf_throttle_set_i,
    input  logic [NrHwPrefetchers-1:0][63:0] icache_hwpf_throttle_i,
    output logic [NrHwPrefetchers-1:0][63:0] icache_hwpf_throttle_o,
    output logic [               63:0]       icache_hwpf_status_o,
    //  }}}

    //  D$
    //  {{{
    //    Cache management
    input logic dcache_enable_i,  // from CSR
    input logic dcache_flush_i,  // high until acknowledged
    output logic                       dcache_flush_ack_o,     // send a single cycle acknowledge signal when the cache is flushed
    output logic dcache_miss_o,  // we missed on a ld/st

    //  AMO interface
    input  ariane_pkg::amo_req_t                     dcache_amo_req_i,    // from LSU
    output ariane_pkg::amo_resp_t                    dcache_amo_resp_o,   // to LSU
    //  CMO interface
    input  cmo_req_t                                 dcache_cmo_req_i,    // from CMO FU
    output cmo_rsp_t                                 dcache_cmo_resp_o,   // to CMO FU
    //  Request ports
    input  ariane_pkg::dcache_req_i_t [NumPorts-1:0] dcache_req_ports_i,  // from LSU
    output ariane_pkg::dcache_req_o_t [NumPorts-1:0] dcache_req_ports_o,  // to LSU
    //  Write Buffer status
    output logic                                     wbuffer_empty_o,
    output logic                                     wbuffer_not_ni_o,

    //  Hardware memory prefetcher configuration
    input  logic [NrHwPrefetchers-1:0]       hwpf_base_set_i,
    input  logic [NrHwPrefetchers-1:0][63:0] hwpf_base_i,
    output logic [NrHwPrefetchers-1:0][63:0] hwpf_base_o,
    input  logic [NrHwPrefetchers-1:0]       hwpf_param_set_i,
    input  logic [NrHwPrefetchers-1:0][63:0] hwpf_param_i,
    output logic [NrHwPrefetchers-1:0][63:0] hwpf_param_o,
    input  logic [NrHwPrefetchers-1:0]       hwpf_throttle_set_i,
    input  logic [NrHwPrefetchers-1:0][63:0] hwpf_throttle_i,
    output logic [NrHwPrefetchers-1:0][63:0] hwpf_throttle_o,
    output logic [               63:0]       hwpf_status_o,

    
    //  AXI port to upstream memory/peripherals
    //  {{{
    output noc_req_t  noc_req_o,
    input  noc_resp_t noc_resp_i
    //  }}}
);
  //  }}}

  `include "axi/typedef.svh"
  `include "hpdcache_typedef.svh"

  //  I$ instantiation
  //  {{{
    localparam int HPICACHE_NREQUESTERS = NumPorts + 2;

  typedef logic [CVA6Cfg.AxiAddrWidth-1:0] hpicache_mem_addr_t;
  typedef logic [ariane_pkg::MEM_TID_WIDTH-1:0] hpicache_mem_id_t;
  typedef logic [CVA6Cfg.AxiDataWidth-1:0] hpicache_mem_data_t;
  typedef logic [CVA6Cfg.AxiDataWidth/8-1:0] hpicache_mem_be_t;
  `HPDCACHE_TYPEDEF_MEM_REQ_T(hpicache_mem_req_t, hpicache_mem_addr_t, hpicache_mem_id_t);
  `HPDCACHE_TYPEDEF_MEM_RESP_R_T(hpicache_mem_resp_r_t, hpicache_mem_id_t, hpicache_mem_data_t);
  `HPDCACHE_TYPEDEF_MEM_REQ_W_T(hpicache_mem_req_w_t, hpicache_mem_data_t, hpicache_mem_be_t);
  `HPDCACHE_TYPEDEF_MEM_RESP_W_T(hpicache_mem_resp_w_t, hpicache_mem_id_t);

  typedef logic [63:0] hwpf_stride_param_t;

  logic                        icache_req_valid[HPICACHE_NREQUESTERS-1:0];
  logic                        icache_req_ready[HPICACHE_NREQUESTERS-1:0];
  hpdcache_pkg::hpdcache_req_t icache_req      [HPICACHE_NREQUESTERS-1:0];
  logic                        icache_req_abort[HPICACHE_NREQUESTERS-1:0];
  hpdcache_pkg::hpdcache_tag_t icache_req_tag  [HPICACHE_NREQUESTERS-1:0];
  hpdcache_pkg::hpdcache_pma_t icache_req_pma  [HPICACHE_NREQUESTERS-1:0];
  logic                        icache_rsp_valid[HPICACHE_NREQUESTERS-1:0];
  hpdcache_pkg::hpdcache_rsp_t icache_rsp      [HPICACHE_NREQUESTERS-1:0];
  logic icache_read_miss, icache_write_miss;

  logic                                   [                2:0] icache_snoop_valid;
  logic                                   [                2:0] icache_snoop_abort;
  hpdcache_pkg::hpdcache_req_offset_t     [                2:0] icache_snoop_addr_offset;
  hpdcache_pkg::hpdcache_tag_t            [                2:0] icache_snoop_addr_tag;
  logic                                   [                2:0] icache_snoop_phys_indexed;

  logic                                                         icache_cmo_req_is_prefetch;

  logic                                                         icache_miss_ready;
  logic                                                         icache_miss_valid;
  hpicache_mem_req_t                                            icache_miss;

  logic                                                         icache_miss_resp_ready;
  logic                                                         icache_miss_resp_valid;
  hpicache_mem_resp_r_t                                         icache_miss_resp;

  logic                                                         icache_wbuf_ready;
  logic                                                         icache_wbuf_valid;
  hpicache_mem_req_t                                            icache_wbuf;

  logic                                                         icache_wbuf_data_ready;
  logic                                                         icache_wbuf_data_valid;
  hpicache_mem_req_w_t                                          icache_wbuf_data;

  logic                                                         icache_wbuf_resp_ready;
  logic                                                         icache_wbuf_resp_valid;
  hpicache_mem_resp_w_t                                         icache_wbuf_resp;

  logic                                                         icache_uc_read_ready;
  logic                                                         icache_uc_read_valid;
  hpicache_mem_req_t                                            icache_uc_read;

  logic                                                         icache_uc_read_resp_ready;
  logic                                                         icache_uc_read_resp_valid;
  hpicache_mem_resp_r_t                                         icache_uc_read_resp;

  logic                                                         icache_uc_write_ready;
  logic                                                         icache_uc_write_valid;
  hpicache_mem_req_t                                            icache_uc_write;

  logic                                                         icache_uc_write_data_ready;
  logic                                                         icache_uc_write_data_valid;
  hpicache_mem_req_w_t                                          icache_uc_write_data;

  logic                                                         icache_uc_write_resp_ready;
  logic                                                         icache_uc_write_resp_valid;
  hpicache_mem_resp_w_t                                         icache_uc_write_resp;

  hwpf_stride_pkg::hwpf_stride_throttle_t [NrHwPrefetchers-1:0] icache_hwpf_throttle_in;
  hwpf_stride_pkg::hwpf_stride_throttle_t [NrHwPrefetchers-1:0] icache_hwpf_throttle_out;

  generate
    ariane_pkg::dcache_req_i_t icache_req_ports[HPICACHE_NREQUESTERS-1:0];

    for (genvar r = 0; r < (NumPorts - 1); r++) begin : cva6_hpdcache_load_if_adapter_gen_icache
      assign icache_req_ports[r] = icache_req_ports_i[r];

      cva6_hpdcache_if_adapter #(
          .CVA6Cfg     (CVA6Cfg),
          .is_load_port(1'b1)
      ) i_cva6_hpicache_load_if_adapter (
          .clk_i,
          .rst_ni,

          .hpdcache_req_sid_i(hpdcache_pkg::hpdcache_req_sid_t'(r)),

          .cva6_req_i     (icache_req_ports[r]),
          .cva6_req_o     (icache_req_ports_o[r]),
          .cva6_amo_req_i ('0),
          .cva6_amo_resp_o(  /* unused */),

          .hpdcache_req_valid_o(icache_req_valid[r]),
          .hpdcache_req_ready_i(icache_req_ready[r]),
          .hpdcache_req_o      (icache_req[r]),
          .hpdcache_req_abort_o(icache_req_abort[r]),
          .hpdcache_req_tag_o  (icache_req_tag[r]),
          .hpdcache_req_pma_o  (icache_req_pma[r]),

          .hpdcache_rsp_valid_i(icache_rsp_valid[r]),
          .hpdcache_rsp_i      (icache_rsp[r])
      );
    end

    cva6_hpdcache_if_adapter #(
        .CVA6Cfg     (CVA6Cfg),
        .is_load_port(1'b0)
    ) i_cva6_hpicache_store_if_adapter (
        .clk_i,
        .rst_ni,

        .hpdcache_req_sid_i(hpdcache_pkg::hpdcache_req_sid_t'(NumPorts - 1)),

        .cva6_req_i     (icache_req_ports_i[NumPorts-1]),
        .cva6_req_o     (icache_req_ports_o[NumPorts-1]),
        .cva6_amo_req_i (icache_amo_req_i),
        .cva6_amo_resp_o(icache_amo_resp_o),

        .hpdcache_req_valid_o(icache_req_valid[NumPorts-1]),
        .hpdcache_req_ready_i(icache_req_ready[NumPorts-1]),
        .hpdcache_req_o      (icache_req[NumPorts-1]),
        .hpdcache_req_abort_o(icache_req_abort[NumPorts-1]),
        .hpdcache_req_tag_o  (icache_req_tag[NumPorts-1]),
        .hpdcache_req_pma_o  (icache_req_pma[NumPorts-1]),

        .hpdcache_rsp_valid_i(icache_rsp_valid[NumPorts-1]),
        .hpdcache_rsp_i      (icache_rsp[NumPorts-1])
    );

`ifdef HPDCACHE_ENABLE_CMO
    cva6_hpdcache_cmo_if_adapter #(
        .cmo_req_t(cmo_req_t),
        .cmo_rsp_t(cmo_rsp_t)
    ) i_cva6_hpicache_cmo_if_adapter (
        .clk_i,
        .rst_ni,

        .dcache_req_sid_i(hpdcache_pkg::hpdcache_req_sid_t'(NumPorts)),

        .cva6_cmo_req_i (icache_cmo_req_i),
        .cva6_cmo_resp_o(icache_cmo_resp_o),

        .dcache_req_valid_o(icache_req_valid[NumPorts]),
        .dcache_req_ready_i(icache_req_ready[NumPorts]),
        .dcache_req_o      (icache_req[NumPorts]),
        .dcache_req_abort_o(icache_req_abort[NumPorts]),
        .dcache_req_tag_o  (icache_req_tag[NumPorts]),
        .dcache_req_pma_o  (icache_req_pma[NumPorts]),

        .dcache_rsp_valid_i(icache_rsp_valid[NumPorts]),
        .dcache_rsp_i      (icache_rsp[NumPorts])
    );
`else
    assign icache_req_valid[NumPorts] = 1'b0,
        icache_req[NumPorts] = '0,
        icache_req_abort[NumPorts] = 1'b0,
        icache_req_tag[NumPorts] = '0,
        icache_req_pma[NumPorts] = '0;
`endif
  endgenerate

  //  Snoop load port
  assign icache_snoop_valid[0] = icache_req_valid[1] & icache_req_ready[1],
      icache_snoop_abort[0] = icache_req_abort[1],
      icache_snoop_addr_offset[0] = icache_req[1].addr_offset,
      icache_snoop_addr_tag[0] = icache_req_tag[1],
      icache_snoop_phys_indexed[0] = icache_req[1].phys_indexed;

  //  Snoop Store/AMO port
  assign icache_snoop_valid[1] = icache_req_valid[NumPorts-1] & icache_req_ready[NumPorts-1],
      icache_snoop_abort[1] = icache_req_abort[NumPorts-1],
      icache_snoop_addr_offset[1] = icache_req[NumPorts-1].addr_offset,
      icache_snoop_addr_tag[1] = icache_req_tag[NumPorts-1],
      icache_snoop_phys_indexed[1] = icache_req[NumPorts-1].phys_indexed;

`ifdef HPDCACHE_ENABLE_CMO
  //  Snoop CMO port (in case of read prefetch accesses)
  assign icache_cmo_req_is_prefetch = hpdcache_pkg::is_cmo_prefetch(
      icache_req[NumPorts].op, icache_req[NumPorts].size
  );
  assign icache_snoop_valid[2]        = icache_req_valid[NumPorts]
                               & icache_req_ready[NumPorts]
                               & icache_cmo_req_is_prefetch,
      icache_snoop_abort[2] = icache_req_abort[NumPorts],
      icache_snoop_addr_offset[2] = icache_req[NumPorts].addr_offset,
      icache_snoop_addr_tag[2] = icache_req_tag[NumPorts],
      icache_snoop_phys_indexed[2] = icache_req[NumPorts].phys_indexed;
`else
  assign icache_snoop_valid[2] = 1'b0,
      icache_snoop_abort[2] = 1'b0,
      icache_snoop_addr_offset[2] = '0,
      icache_snoop_addr_tag[2] = '0,
      icache_snoop_phys_indexed[2] = 1'b0;
`endif

  generate
    for (genvar h = 0; h < NrHwPrefetchers; h++) begin : hwpf_throttle_gen_icache
      assign icache_hwpf_throttle_in[h] = hwpf_stride_pkg::hwpf_stride_throttle_t'(icache_hwpf_throttle_i[h]),
          icache_hwpf_throttle_o[h] = hwpf_stride_pkg::hwpf_stride_param_t'(icache_hwpf_throttle_out[h]);
    end
  endgenerate

  hwpf_stride_wrapper #(
      .NUM_HW_PREFETCH(NrHwPrefetchers),
      .NUM_SNOOP_PORTS(3)
  ) i_hwpf_stride_wrapper_icache (
      .clk_i,
      .rst_ni,

      .hwpf_stride_base_set_i    (icache_hwpf_base_set_i),
      .hwpf_stride_base_i        (icache_hwpf_base_i),
      .hwpf_stride_base_o        (icache_hwpf_base_o),
      .hwpf_stride_param_set_i   (icache_hwpf_param_set_i),
      .hwpf_stride_param_i       (icache_hwpf_param_i),
      .hwpf_stride_param_o       (icache_hwpf_param_o),
      .hwpf_stride_throttle_set_i(icache_hwpf_throttle_set_i),
      .hwpf_stride_throttle_i    (icache_hwpf_throttle_in),
      .hwpf_stride_throttle_o    (icache_hwpf_throttle_out),
      .hwpf_stride_status_o      (icache_hwpf_status_o),

      .snoop_valid_i       (icache_snoop_valid),
      .snoop_abort_i       (icache_snoop_abort),
      .snoop_addr_offset_i (icache_snoop_addr_offset),
      .snoop_addr_tag_i    (icache_snoop_addr_tag),
      .snoop_phys_indexed_i(icache_snoop_phys_indexed),

      .hpdcache_req_sid_i(hpdcache_pkg::hpdcache_req_sid_t'(NumPorts + 1)),

      .hpdcache_req_valid_o(icache_req_valid[NumPorts+1]),
      .hpdcache_req_ready_i(icache_req_ready[NumPorts+1]),
      .hpdcache_req_o      (icache_req[NumPorts+1]),
      .hpdcache_req_abort_o(icache_req_abort[NumPorts+1]),
      .hpdcache_req_tag_o  (icache_req_tag[NumPorts+1]),
      .hpdcache_req_pma_o  (icache_req_pma[NumPorts+1]),
      .hpdcache_rsp_valid_i(icache_rsp_valid[NumPorts+1]),
      .hpdcache_rsp_i      (icache_rsp[NumPorts+1])
  );

hpdcache #(
      .NREQUESTERS          (HPICACHE_NREQUESTERS),
      .HPDcacheMemIdWidth   (ariane_pkg::MEM_TID_WIDTH),
      .HPDcacheMemDataWidth (CVA6Cfg.AxiDataWidth),
      .hpdcache_mem_req_t   (hpicache_mem_req_t),
      .hpdcache_mem_req_w_t (hpicache_mem_req_w_t),
      .hpdcache_mem_resp_r_t(hpicache_mem_resp_r_t),
      .hpdcache_mem_resp_w_t(hpicache_mem_resp_w_t)
  ) i_hpicache (
      .clk_i,
      .rst_ni,

      .wbuf_flush_i(icache_flush_i),

      .core_req_valid_i(icache_req_valid),
      .core_req_ready_o(icache_req_ready),
      .core_req_i      (icache_req),
      .core_req_abort_i(icache_req_abort),
      .core_req_tag_i  (icache_req_tag),
      .core_req_pma_i  (icache_req_pma),

      .core_rsp_valid_o(icache_rsp_valid),
      .core_rsp_o      (icache_rsp),

      .mem_req_miss_read_ready_i(icache_miss_ready),
      .mem_req_miss_read_valid_o(icache_miss_valid),
      .mem_req_miss_read_o      (icache_miss),

      .mem_resp_miss_read_ready_o(icache_miss_resp_ready),
      .mem_resp_miss_read_valid_i(icache_miss_resp_valid),
      .mem_resp_miss_read_i      (icache_miss_resp),

      .mem_req_wbuf_write_ready_i(icache_wbuf_ready),
      .mem_req_wbuf_write_valid_o(icache_wbuf_valid),
      .mem_req_wbuf_write_o      (icache_wbuf),

      .mem_req_wbuf_write_data_ready_i(icache_wbuf_data_ready),
      .mem_req_wbuf_write_data_valid_o(icache_wbuf_data_valid),
      .mem_req_wbuf_write_data_o      (icache_wbuf_data),

      .mem_resp_wbuf_write_ready_o(icache_wbuf_resp_ready),
      .mem_resp_wbuf_write_valid_i(icache_wbuf_resp_valid),
      .mem_resp_wbuf_write_i      (icache_wbuf_resp),

      .mem_req_uc_read_ready_i(icache_uc_read_ready),
      .mem_req_uc_read_valid_o(icache_uc_read_valid),
      .mem_req_uc_read_o      (icache_uc_read),

      .mem_resp_uc_read_ready_o(icache_uc_read_resp_ready),
      .mem_resp_uc_read_valid_i(icache_uc_read_resp_valid),
      .mem_resp_uc_read_i      (icache_uc_read_resp),

      .mem_req_uc_write_ready_i(icache_uc_write_ready),
      .mem_req_uc_write_valid_o(icache_uc_write_valid),
      .mem_req_uc_write_o      (icache_uc_write),

      .mem_req_uc_write_data_ready_i(icache_uc_write_data_ready),
      .mem_req_uc_write_data_valid_o(icache_uc_write_data_valid),
      .mem_req_uc_write_data_o      (icache_uc_write_data),

      .mem_resp_uc_write_ready_o(icache_uc_write_resp_ready),
      .mem_resp_uc_write_valid_i(icache_uc_write_resp_valid),
      .mem_resp_uc_write_i      (icache_uc_write_resp),

      .evt_cache_write_miss_o(icache_write_miss),
      .evt_cache_read_miss_o (icache_read_miss),
      .evt_uncached_req_o    (  /* unused */),
      .evt_cmo_req_o         (  /* unused */),
      .evt_write_req_o       (  /* unused */),
      .evt_read_req_o        (  /* unused */),
      .evt_prefetch_req_o    (  /* unused */),
      .evt_req_on_hold_o     (  /* unused */),
      .evt_rtab_rollback_o   (  /* unused */),
      .evt_stall_refill_o    (  /* unused */),
      .evt_stall_o           (  /* unused */),

      .wbuf_empty_o(icache_wbuffer_empty_o),

      .cfg_enable_i                       (icache_enable_i),
      .cfg_wbuf_threshold_i               (4'd2),
      .cfg_wbuf_reset_timecnt_on_write_i  (1'b1),
      .cfg_wbuf_sequential_waw_i          (1'b0),
      .cfg_wbuf_inhibit_write_coalescing_i(1'b0),
      .cfg_prefetch_updt_plru_i           (1'b1),
      .cfg_error_on_cacheable_amo_i       (1'b0),
      .cfg_rtab_single_entry_i            (1'b0)
  );

assign icache_miss_o = icache_read_miss, icache_wbuffer_not_ni_o = icache_wbuffer_empty_o;

  always_ff @(posedge clk_i or negedge rst_ni) begin : icache_flush_ff
    if (!rst_ni) icache_flush_ack_o <= 1'b0;
    else icache_flush_ack_o <= ~icache_flush_ack_o & icache_flush_i;
  end

//  D$ instantiation
  //  {{{
  `include "hpdcache_typedef.svh"

  //    0: Page-Table Walk (PTW)
  //    1: Load unit
  //    2: Accelerator load
  //    3: Store/AMO
  //    .
  //    .
  //    .
  //    NumPorts: CMO
  //    NumPorts + 1: Hardware Memory Prefetcher (hwpf)
  localparam int HPDCACHE_NREQUESTERS = NumPorts + 2;

  typedef logic [CVA6Cfg.AxiAddrWidth-1:0] hpdcache_mem_addr_t;
  typedef logic [ariane_pkg::MEM_TID_WIDTH-1:0] hpdcache_mem_id_t;
  typedef logic [CVA6Cfg.AxiDataWidth-1:0] hpdcache_mem_data_t;
  typedef logic [CVA6Cfg.AxiDataWidth/8-1:0] hpdcache_mem_be_t;
  `HPDCACHE_TYPEDEF_MEM_REQ_T(hpdcache_mem_req_t, hpdcache_mem_addr_t, hpdcache_mem_id_t);
  `HPDCACHE_TYPEDEF_MEM_RESP_R_T(hpdcache_mem_resp_r_t, hpdcache_mem_id_t, hpdcache_mem_data_t);
  `HPDCACHE_TYPEDEF_MEM_REQ_W_T(hpdcache_mem_req_w_t, hpdcache_mem_data_t, hpdcache_mem_be_t);
  `HPDCACHE_TYPEDEF_MEM_RESP_W_T(hpdcache_mem_resp_w_t, hpdcache_mem_id_t);

  //typedef logic [63:0] hwpf_stride_param_t;

  logic                        dcache_req_valid[HPDCACHE_NREQUESTERS-1:0];
  logic                        dcache_req_ready[HPDCACHE_NREQUESTERS-1:0];
  hpdcache_pkg::hpdcache_req_t dcache_req      [HPDCACHE_NREQUESTERS-1:0];
  logic                        dcache_req_abort[HPDCACHE_NREQUESTERS-1:0];
  hpdcache_pkg::hpdcache_tag_t dcache_req_tag  [HPDCACHE_NREQUESTERS-1:0];
  hpdcache_pkg::hpdcache_pma_t dcache_req_pma  [HPDCACHE_NREQUESTERS-1:0];
  logic                        dcache_rsp_valid[HPDCACHE_NREQUESTERS-1:0];
  hpdcache_pkg::hpdcache_rsp_t dcache_rsp      [HPDCACHE_NREQUESTERS-1:0];
  logic dcache_read_miss, dcache_write_miss;

  logic                                   [                2:0] snoop_valid;
  logic                                   [                2:0] snoop_abort;
  hpdcache_pkg::hpdcache_req_offset_t     [                2:0] snoop_addr_offset;
  hpdcache_pkg::hpdcache_tag_t            [                2:0] snoop_addr_tag;
  logic                                   [                2:0] snoop_phys_indexed;

  logic                                                         dcache_cmo_req_is_prefetch;

  logic                                                         dcache_miss_ready;
  logic                                                         dcache_miss_valid;
  hpdcache_mem_req_t                                            dcache_miss;

  logic                                                         dcache_miss_resp_ready;
  logic                                                         dcache_miss_resp_valid;
  hpdcache_mem_resp_r_t                                         dcache_miss_resp;

  logic                                                         dcache_wbuf_ready;
  logic                                                         dcache_wbuf_valid;
  hpdcache_mem_req_t                                            dcache_wbuf;

  logic                                                         dcache_wbuf_data_ready;
  logic                                                         dcache_wbuf_data_valid;
  hpdcache_mem_req_w_t                                          dcache_wbuf_data;

  logic                                                         dcache_wbuf_resp_ready;
  logic                                                         dcache_wbuf_resp_valid;
  hpdcache_mem_resp_w_t                                         dcache_wbuf_resp;

  logic                                                         dcache_uc_read_ready;
  logic                                                         dcache_uc_read_valid;
  hpdcache_mem_req_t                                            dcache_uc_read;

  logic                                                         dcache_uc_read_resp_ready;
  logic                                                         dcache_uc_read_resp_valid;
  hpdcache_mem_resp_r_t                                         dcache_uc_read_resp;

  logic                                                         dcache_uc_write_ready;
  logic                                                         dcache_uc_write_valid;
  hpdcache_mem_req_t                                            dcache_uc_write;

  logic                                                         dcache_uc_write_data_ready;
  logic                                                         dcache_uc_write_data_valid;
  hpdcache_mem_req_w_t                                          dcache_uc_write_data;

  logic                                                         dcache_uc_write_resp_ready;
  logic                                                         dcache_uc_write_resp_valid;
  hpdcache_mem_resp_w_t                                         dcache_uc_write_resp;

  hwpf_stride_pkg::hwpf_stride_throttle_t [NrHwPrefetchers-1:0] hwpf_throttle_in;
  hwpf_stride_pkg::hwpf_stride_throttle_t [NrHwPrefetchers-1:0] hwpf_throttle_out;

  generate
    ariane_pkg::dcache_req_i_t dcache_req_ports[HPDCACHE_NREQUESTERS-1:0];

    for (genvar r = 0; r < (NumPorts - 1); r++) begin : cva6_hpdcache_load_if_adapter_gen_dcache
      assign dcache_req_ports[r] = dcache_req_ports_i[r];

      cva6_hpdcache_if_adapter #(
          .CVA6Cfg     (CVA6Cfg),
          .is_load_port(1'b1)
      ) i_cva6_hpdcache_load_if_adapter (
          .clk_i,
          .rst_ni,

          .hpdcache_req_sid_i(hpdcache_pkg::hpdcache_req_sid_t'(r)),

          .cva6_req_i     (dcache_req_ports[r]),
          .cva6_req_o     (dcache_req_ports_o[r]),
          .cva6_amo_req_i ('0),
          .cva6_amo_resp_o(  /* unused */),

          .hpdcache_req_valid_o(dcache_req_valid[r]),
          .hpdcache_req_ready_i(dcache_req_ready[r]),
          .hpdcache_req_o      (dcache_req[r]),
          .hpdcache_req_abort_o(dcache_req_abort[r]),
          .hpdcache_req_tag_o  (dcache_req_tag[r]),
          .hpdcache_req_pma_o  (dcache_req_pma[r]),

          .hpdcache_rsp_valid_i(dcache_rsp_valid[r]),
          .hpdcache_rsp_i      (dcache_rsp[r])
      );
    end

    cva6_hpdcache_if_adapter #(
        .CVA6Cfg     (CVA6Cfg),
        .is_load_port(1'b0)
    ) i_cva6_hpdcache_store_if_adapter (
        .clk_i,
        .rst_ni,

        .hpdcache_req_sid_i(hpdcache_pkg::hpdcache_req_sid_t'(NumPorts - 1)),

        .cva6_req_i     (dcache_req_ports_i[NumPorts-1]),
        .cva6_req_o     (dcache_req_ports_o[NumPorts-1]),
        .cva6_amo_req_i (dcache_amo_req_i),
        .cva6_amo_resp_o(dcache_amo_resp_o),

        .hpdcache_req_valid_o(dcache_req_valid[NumPorts-1]),
        .hpdcache_req_ready_i(dcache_req_ready[NumPorts-1]),
        .hpdcache_req_o      (dcache_req[NumPorts-1]),
        .hpdcache_req_abort_o(dcache_req_abort[NumPorts-1]),
        .hpdcache_req_tag_o  (dcache_req_tag[NumPorts-1]),
        .hpdcache_req_pma_o  (dcache_req_pma[NumPorts-1]),

        .hpdcache_rsp_valid_i(dcache_rsp_valid[NumPorts-1]),
        .hpdcache_rsp_i      (dcache_rsp[NumPorts-1])
    );

`ifdef HPDCACHE_ENABLE_CMO
    cva6_hpdcache_cmo_if_adapter #(
        .cmo_req_t(cmo_req_t),
        .cmo_rsp_t(cmo_rsp_t)
    ) i_cva6_hpdcache_cmo_if_adapter (
        .clk_i,
        .rst_ni,

        .dcache_req_sid_i(hpdcache_pkg::hpdcache_req_sid_t'(NumPorts)),

        .cva6_cmo_req_i (dcache_cmo_req_i),
        .cva6_cmo_resp_o(dcache_cmo_resp_o),

        .dcache_req_valid_o(dcache_req_valid[NumPorts]),
        .dcache_req_ready_i(dcache_req_ready[NumPorts]),
        .dcache_req_o      (dcache_req[NumPorts]),
        .dcache_req_abort_o(dcache_req_abort[NumPorts]),
        .dcache_req_tag_o  (dcache_req_tag[NumPorts]),
        .dcache_req_pma_o  (dcache_req_pma[NumPorts]),

        .dcache_rsp_valid_i(dcache_rsp_valid[NumPorts]),
        .dcache_rsp_i      (dcache_rsp[NumPorts])
    );
`else
    assign dcache_req_valid[NumPorts] = 1'b0,
        dcache_req[NumPorts] = '0,
        dcache_req_abort[NumPorts] = 1'b0,
        dcache_req_tag[NumPorts] = '0,
        dcache_req_pma[NumPorts] = '0;
`endif
  endgenerate

  //  Snoop load port
  assign snoop_valid[0] = dcache_req_valid[1] & dcache_req_ready[1],
      snoop_abort[0] = dcache_req_abort[1],
      snoop_addr_offset[0] = dcache_req[1].addr_offset,
      snoop_addr_tag[0] = dcache_req_tag[1],
      snoop_phys_indexed[0] = dcache_req[1].phys_indexed;

  //  Snoop Store/AMO port
  assign snoop_valid[1] = dcache_req_valid[NumPorts-1] & dcache_req_ready[NumPorts-1],
      snoop_abort[1] = dcache_req_abort[NumPorts-1],
      snoop_addr_offset[1] = dcache_req[NumPorts-1].addr_offset,
      snoop_addr_tag[1] = dcache_req_tag[NumPorts-1],
      snoop_phys_indexed[1] = dcache_req[NumPorts-1].phys_indexed;

`ifdef HPDCACHE_ENABLE_CMO
  //  Snoop CMO port (in case of read prefetch accesses)
  assign dcache_cmo_req_is_prefetch = hpdcache_pkg::is_cmo_prefetch(
      dcache_req[NumPorts].op, dcache_req[NumPorts].size
  );
  assign snoop_valid[2]        = dcache_req_valid[NumPorts]
                               & dcache_req_ready[NumPorts]
                               & dcache_cmo_req_is_prefetch,
      snoop_abort[2] = dcache_req_abort[NumPorts],
      snoop_addr_offset[2] = dcache_req[NumPorts].addr_offset,
      snoop_addr_tag[2] = dcache_req_tag[NumPorts],
      snoop_phys_indexed[2] = dcache_req[NumPorts].phys_indexed;
`else
  assign snoop_valid[2] = 1'b0,
      snoop_abort[2] = 1'b0,
      snoop_addr_offset[2] = '0,
      snoop_addr_tag[2] = '0,
      snoop_phys_indexed[2] = 1'b0;
`endif

  generate
    for (genvar h = 0; h < NrHwPrefetchers; h++) begin : hwpf_throttle_gen_dcache
      assign hwpf_throttle_in[h] = hwpf_stride_pkg::hwpf_stride_throttle_t'(hwpf_throttle_i[h]),
          hwpf_throttle_o[h] = hwpf_stride_pkg::hwpf_stride_param_t'(hwpf_throttle_out[h]);
    end
  endgenerate

  hwpf_stride_wrapper #(
      .NUM_HW_PREFETCH(NrHwPrefetchers),
      .NUM_SNOOP_PORTS(3)
  ) i_hwpf_stride_wrapper (
      .clk_i,
      .rst_ni,

      .hwpf_stride_base_set_i    (hwpf_base_set_i),
      .hwpf_stride_base_i        (hwpf_base_i),
      .hwpf_stride_base_o        (hwpf_base_o),
      .hwpf_stride_param_set_i   (hwpf_param_set_i),
      .hwpf_stride_param_i       (hwpf_param_i),
      .hwpf_stride_param_o       (hwpf_param_o),
      .hwpf_stride_throttle_set_i(hwpf_throttle_set_i),
      .hwpf_stride_throttle_i    (hwpf_throttle_in),
      .hwpf_stride_throttle_o    (hwpf_throttle_out),
      .hwpf_stride_status_o      (hwpf_status_o),

      .snoop_valid_i       (snoop_valid),
      .snoop_abort_i       (snoop_abort),
      .snoop_addr_offset_i (snoop_addr_offset),
      .snoop_addr_tag_i    (snoop_addr_tag),
      .snoop_phys_indexed_i(snoop_phys_indexed),

      .hpdcache_req_sid_i(hpdcache_pkg::hpdcache_req_sid_t'(NumPorts + 1)),

      .hpdcache_req_valid_o(dcache_req_valid[NumPorts+1]),
      .hpdcache_req_ready_i(dcache_req_ready[NumPorts+1]),
      .hpdcache_req_o      (dcache_req[NumPorts+1]),
      .hpdcache_req_abort_o(dcache_req_abort[NumPorts+1]),
      .hpdcache_req_tag_o  (dcache_req_tag[NumPorts+1]),
      .hpdcache_req_pma_o  (dcache_req_pma[NumPorts+1]),
      .hpdcache_rsp_valid_i(dcache_rsp_valid[NumPorts+1]),
      .hpdcache_rsp_i      (dcache_rsp[NumPorts+1])
  );

  hpdcache #(
      .NREQUESTERS          (HPDCACHE_NREQUESTERS),
      .HPDcacheMemIdWidth   (ariane_pkg::MEM_TID_WIDTH),
      .HPDcacheMemDataWidth (CVA6Cfg.AxiDataWidth),
      .hpdcache_mem_req_t   (hpdcache_mem_req_t),
      .hpdcache_mem_req_w_t (hpdcache_mem_req_w_t),
      .hpdcache_mem_resp_r_t(hpdcache_mem_resp_r_t),
      .hpdcache_mem_resp_w_t(hpdcache_mem_resp_w_t)
  ) i_hpdcache (
      .clk_i,
      .rst_ni,

      .wbuf_flush_i(dcache_flush_i),

      .core_req_valid_i(dcache_req_valid),
      .core_req_ready_o(dcache_req_ready),
      .core_req_i      (dcache_req),
      .core_req_abort_i(dcache_req_abort),
      .core_req_tag_i  (dcache_req_tag),
      .core_req_pma_i  (dcache_req_pma),

      .core_rsp_valid_o(dcache_rsp_valid),
      .core_rsp_o      (dcache_rsp),

      .mem_req_miss_read_ready_i(dcache_miss_ready),
      .mem_req_miss_read_valid_o(dcache_miss_valid),
      .mem_req_miss_read_o      (dcache_miss),

      .mem_resp_miss_read_ready_o(dcache_miss_resp_ready),
      .mem_resp_miss_read_valid_i(dcache_miss_resp_valid),
      .mem_resp_miss_read_i      (dcache_miss_resp),

      .mem_req_wbuf_write_ready_i(dcache_wbuf_ready),
      .mem_req_wbuf_write_valid_o(dcache_wbuf_valid),
      .mem_req_wbuf_write_o      (dcache_wbuf),

      .mem_req_wbuf_write_data_ready_i(dcache_wbuf_data_ready),
      .mem_req_wbuf_write_data_valid_o(dcache_wbuf_data_valid),
      .mem_req_wbuf_write_data_o      (dcache_wbuf_data),

      .mem_resp_wbuf_write_ready_o(dcache_wbuf_resp_ready),
      .mem_resp_wbuf_write_valid_i(dcache_wbuf_resp_valid),
      .mem_resp_wbuf_write_i      (dcache_wbuf_resp),

      .mem_req_uc_read_ready_i(dcache_uc_read_ready),
      .mem_req_uc_read_valid_o(dcache_uc_read_valid),
      .mem_req_uc_read_o      (dcache_uc_read),

      .mem_resp_uc_read_ready_o(dcache_uc_read_resp_ready),
      .mem_resp_uc_read_valid_i(dcache_uc_read_resp_valid),
      .mem_resp_uc_read_i      (dcache_uc_read_resp),

      .mem_req_uc_write_ready_i(dcache_uc_write_ready),
      .mem_req_uc_write_valid_o(dcache_uc_write_valid),
      .mem_req_uc_write_o      (dcache_uc_write),

      .mem_req_uc_write_data_ready_i(dcache_uc_write_data_ready),
      .mem_req_uc_write_data_valid_o(dcache_uc_write_data_valid),
      .mem_req_uc_write_data_o      (dcache_uc_write_data),

      .mem_resp_uc_write_ready_o(dcache_uc_write_resp_ready),
      .mem_resp_uc_write_valid_i(dcache_uc_write_resp_valid),
      .mem_resp_uc_write_i      (dcache_uc_write_resp),

      .evt_cache_write_miss_o(dcache_write_miss),
      .evt_cache_read_miss_o (dcache_read_miss),
      .evt_uncached_req_o    (  /* unused */),
      .evt_cmo_req_o         (  /* unused */),
      .evt_write_req_o       (  /* unused */),
      .evt_read_req_o        (  /* unused */),
      .evt_prefetch_req_o    (  /* unused */),
      .evt_req_on_hold_o     (  /* unused */),
      .evt_rtab_rollback_o   (  /* unused */),
      .evt_stall_refill_o    (  /* unused */),
      .evt_stall_o           (  /* unused */),

      .wbuf_empty_o(wbuffer_empty_o),

      .cfg_enable_i                       (dcache_enable_i),
      .cfg_wbuf_threshold_i               (4'd2),
      .cfg_wbuf_reset_timecnt_on_write_i  (1'b1),
      .cfg_wbuf_sequential_waw_i          (1'b0),
      .cfg_wbuf_inhibit_write_coalescing_i(1'b0),
      .cfg_prefetch_updt_plru_i           (1'b1),
      .cfg_error_on_cacheable_amo_i       (1'b0),
      .cfg_rtab_single_entry_i            (1'b0)
  );

  assign dcache_miss_o = dcache_read_miss, wbuffer_not_ni_o = wbuffer_empty_o;

  always_ff @(posedge clk_i or negedge rst_ni) begin : dcache_flush_ff
    if (!rst_ni) dcache_flush_ack_o <= 1'b0;
    else dcache_flush_ack_o <= ~dcache_flush_ack_o & dcache_flush_i;
  end

  //  }}}

  //  AXI arbiter instantiation
  //  {{{
  typedef logic [CVA6Cfg.AxiAddrWidth-1:0] axi_addr_t;
  typedef logic [CVA6Cfg.AxiDataWidth-1:0] axi_data_t;
  typedef logic [CVA6Cfg.AxiDataWidth/8-1:0] axi_strb_t;
  typedef logic [CVA6Cfg.AxiIdWidth-1:0] axi_id_t;
  typedef logic [CVA6Cfg.AxiUserWidth-1:0] axi_user_t;
  `AXI_TYPEDEF_AW_CHAN_T(axi_aw_chan_t, axi_addr_t, axi_id_t, axi_user_t)
  `AXI_TYPEDEF_W_CHAN_T(axi_w_chan_t, axi_data_t, axi_strb_t, axi_user_t)
  `AXI_TYPEDEF_B_CHAN_T(axi_b_chan_t, axi_id_t, axi_user_t)
  `AXI_TYPEDEF_AR_CHAN_T(axi_ar_chan_t, axi_addr_t, axi_id_t, axi_user_t)
  `AXI_TYPEDEF_R_CHAN_T(axi_r_chan_t, axi_data_t, axi_id_t, axi_user_t)


// old icache related items
 localparam int ICACHE_RDTXID = 1 << (ariane_pkg::MEM_TID_WIDTH - 1);

 // arbiter implemented with old icache signals
  cva6_hpdcache_subsystem_axi_arbiter #(
      .HPDcacheMemIdWidth   (ariane_pkg::MEM_TID_WIDTH),
      .HPDcacheMemDataWidth (CVA6Cfg.AxiDataWidth),
      .hpdcache_mem_req_t   (hpdcache_mem_req_t),
      .hpdcache_mem_req_w_t (hpdcache_mem_req_w_t),
      .hpdcache_mem_resp_r_t(hpdcache_mem_resp_r_t),
      .hpdcache_mem_resp_w_t(hpdcache_mem_resp_w_t),

      .AxiAddrWidth (CVA6Cfg.AxiAddrWidth),
      .AxiDataWidth (CVA6Cfg.AxiDataWidth),
      .AxiIdWidth   (CVA6Cfg.AxiIdWidth),
      .AxiUserWidth (CVA6Cfg.AxiUserWidth),
      .axi_ar_chan_t(axi_ar_chan_t),
      .axi_aw_chan_t(axi_aw_chan_t),
      .axi_w_chan_t (axi_w_chan_t),
      .axi_req_t    (noc_req_t),
      .axi_rsp_t    (noc_resp_t)
  ) i_axi_arbiter (
      .clk_i,
      .rst_ni,

       // old cache ports
       .icache_miss_valid_i(icache_miss_valid),
       .icache_miss_ready_o(icache_miss_ready),
       .icache_miss_i      (icache_miss),
       .icache_miss_id_i   (hpdcache_mem_id_t'(ICACHE_RDTXID)),

       .icache_miss_resp_valid_o(icache_miss_resp_valid),
       .icache_miss_resp_o      (icache_miss_resp),

       // hp icache signals
    //   .icache_miss_ready_o(icache_miss_ready),
    //   .icache_miss_valid_i(icache_miss_valid),
    //   .icache_miss_i      (icache_miss),

    //   .icache_miss_resp_ready_i(icache_miss_resp_ready),
    //   .icache_miss_resp_valid_o(icache_miss_resp_valid),
    //   .icache_miss_resp_o      (icache_miss_resp),

    //   .icache_wbuf_ready_o(icache_wbuf_ready),
    //   .icache_wbuf_valid_i(icache_wbuf_valid),
    //   .icache_wbuf_i      (icache_wbuf),

    //   .icache_wbuf_data_ready_o(icache_wbuf_data_ready),
    //   .icache_wbuf_data_valid_i(icache_wbuf_data_valid),
    //   .icache_wbuf_data_i      (icache_wbuf_data),

    //   .icache_wbuf_resp_ready_i(icache_wbuf_resp_ready),
    //   .icache_wbuf_resp_valid_o(icache_wbuf_resp_valid),
    //   .icache_wbuf_resp_o      (icache_wbuf_resp),

    //   .icache_uc_read_ready_o(icache_uc_read_ready),
    //   .icache_uc_read_valid_i(icache_uc_read_valid),
    //   .icache_uc_read_i      (icache_uc_read),
    //   .icache_uc_read_id_i   ('1),

    //   .icache_uc_read_resp_ready_i(icache_uc_read_resp_ready),
    //   .icache_uc_read_resp_valid_o(icache_uc_read_resp_valid),
    //   .icache_uc_read_resp_o      (icache_uc_read_resp),

    //   .icache_uc_write_ready_o(icache_uc_write_ready),
    //   .icache_uc_write_valid_i(icache_uc_write_valid),
    //   .icache_uc_write_i      (icache_uc_write),
    //   .icache_uc_write_id_i   ('1),

    //   .icache_uc_write_data_ready_o(icache_uc_write_data_ready),
    //   .icache_uc_write_data_valid_i(icache_uc_write_data_valid),
    //   .icache_uc_write_data_i      (icache_uc_write_data),

    //   .icache_uc_write_resp_ready_i(icache_uc_write_resp_ready),
    //   .icache_uc_write_resp_valid_o(icache_uc_write_resp_valid),
    //   .icache_uc_write_resp_o      (icache_uc_write_resp),

      .dcache_miss_ready_o(dcache_miss_ready),
      .dcache_miss_valid_i(dcache_miss_valid),
      .dcache_miss_i      (dcache_miss),

      .dcache_miss_resp_ready_i(dcache_miss_resp_ready),
      .dcache_miss_resp_valid_o(dcache_miss_resp_valid),
      .dcache_miss_resp_o      (dcache_miss_resp),

      .dcache_wbuf_ready_o(dcache_wbuf_ready),
      .dcache_wbuf_valid_i(dcache_wbuf_valid),
      .dcache_wbuf_i      (dcache_wbuf),

      .dcache_wbuf_data_ready_o(dcache_wbuf_data_ready),
      .dcache_wbuf_data_valid_i(dcache_wbuf_data_valid),
      .dcache_wbuf_data_i      (dcache_wbuf_data),

      .dcache_wbuf_resp_ready_i(dcache_wbuf_resp_ready),
      .dcache_wbuf_resp_valid_o(dcache_wbuf_resp_valid),
      .dcache_wbuf_resp_o      (dcache_wbuf_resp),

      .dcache_uc_read_ready_o(dcache_uc_read_ready),
      .dcache_uc_read_valid_i(dcache_uc_read_valid),
      .dcache_uc_read_i      (dcache_uc_read),
      .dcache_uc_read_id_i   ('1),

      .dcache_uc_read_resp_ready_i(dcache_uc_read_resp_ready),
      .dcache_uc_read_resp_valid_o(dcache_uc_read_resp_valid),
      .dcache_uc_read_resp_o      (dcache_uc_read_resp),

      .dcache_uc_write_ready_o(dcache_uc_write_ready),
      .dcache_uc_write_valid_i(dcache_uc_write_valid),
      .dcache_uc_write_i      (dcache_uc_write),
      .dcache_uc_write_id_i   ('1),

      .dcache_uc_write_data_ready_o(dcache_uc_write_data_ready),
      .dcache_uc_write_data_valid_i(dcache_uc_write_data_valid),
      .dcache_uc_write_data_i      (dcache_uc_write_data),

      .dcache_uc_write_resp_ready_i(dcache_uc_write_resp_ready),
      .dcache_uc_write_resp_valid_o(dcache_uc_write_resp_valid),
      .dcache_uc_write_resp_o      (dcache_uc_write_resp),

      .axi_req_o (noc_req_o),
      .axi_resp_i(noc_resp_i)
  );
endmodule: cva6_hpdcache_subsystem
  //  }}}