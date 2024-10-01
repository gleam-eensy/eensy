// type NetworkTupleKeys {
//   Ap
//   Psk
//   Ssid
//   ApChannel
//   ApSsidHidden
//   ApMaxConnections
// }

// -type ap_config_property() ::
//     ssid_config()
//     | psk_config()
//     | ap_channel_cfg()
//     | ap_ssid_hidden_config()
//     | ap_max_connections_config()
//     | ap_started_config()
//     | ap_sta_connected_config()
//     | ap_sta_disconnected_config()
//     | ap_sta_ip_assigned_config().
// -type ap_config() :: {ap, [ap_config_property()]}.

// -type ssid_config() :: {ssid, string() | binary()}.
// -type psk_config() :: {psk, string() | binary()}.
// -type ap_channel_cfg() :: {ap_channel, wifi_channel()}.
// -type ap_ssid_hidden_config() :: {ap_ssid_hidden, boolean()}.
// -type ap_max_connections_config() :: {ap_max_connections, non_neg_integer()}.
// -type ap_started_config() :: {ap_started, fun(() -> term())}.
// -type ap_sta_connected_config() :: {sta_connected, fun((mac()) -> term())}.
// -type ap_sta_disconnected_config() :: {sta_disconnected, fun((mac()) -> term())}.
// -type ap_sta_ip_assigned_config() :: {sta_ip_assigned, fun((ipv4_address()) -> term())}.
// -type ap_config_property() ::
//     ssid_config()
//     | psk_config()
//     | ap_channel_cfg()
//     | ap_ssid_hidden_config()
//     | ap_max_connections_config()
//     | ap_started_config()
//     | ap_sta_connected_config()
//     | ap_sta_disconnected_config()
//     | ap_sta_ip_assigned_config().
// -type ap_config() :: {ap, [ap_config_property()]}.

/// Start Access Point in unsecure mode
@external(erlang, "eensy_ffi", "wait_for_ap_with_result")
pub fn start() -> Result(Nil, Nil)
