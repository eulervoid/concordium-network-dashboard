module Explorer.Stubs exposing (..)


blockSummaryStubs =
    [ ( "allTransactions", getBlockSummary_stub_allTransactions )
    , ( "module deploys", getBlockSummary_stub_contractDeploys )
    , ( "failure", getBlockSummary_stub_failure )
    , ( "rstub", getBlockSummaryResponseStub )
    ]


getBlockSummary_stub_allTransactions =
    """
{
  "specialEvents": [
    {
      "bakerId": 1,
      "rewardAmount": 12463,
      "bakerAccount": "3siDnxannkQYYjCTgwEUvE9WThEaHy1J3RjyMA4ZBQmrR9hw1K"
    }
  ],
  "transactionSummaries": [
    {
      "hash": "ae3154e1ee8a64d9e9718e57fbc979175be2ee8526b23fcddab2d626ebaeb72d",
      "sender": null,
      "cost": 0,
      "result": {
        "events": [
          {
            "tag": "AccountCreated",
            "contents": "3P2Qsi8FeMB6AijMi3kpD9FrcFfAkoHDMu8kf7Fbd3cYYJpk2s"
          },
          {
            "tag": "CredentialDeployed",
            "regid": "ac64aac1da15afd90d185475705935fbe224d5f7ddc43988e7db511656c787b452820b5096a2b323cf7612f5609d4cfb",
            "account": "3P2Qsi8FeMB6AijMi3kpD9FrcFfAkoHDMu8kf7Fbd3cYYJpk2s"
          }
        ],
        "outcome": "success"
      },
      "energyCost": 35000,
      "type": null,
      "index": 0
    },
    {
      "hash": "4b7e1b22ff9365e29c57d7f85a0fd7a7dd3c50245cb9d04c560c520298013c3f",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 165,
      "energyCost": 165,
      "result": {
        "events": [
          {
            "amount": 1000000,
            "tag": "Transferred",
            "to": {
              "address": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
              "type": "AddressAccount"
            },
            "from": {
              "address": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "type": "transfer",
      "index": 0
    },
    {
      "hash": "15c2c8a0a9d496630dff603d4d404a6912d96215755884522798092bc179de5b",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 265,
      "energyCost": 265,
      "result": {
        "events": [
          {
            "tag": "StakeDelegated",
            "account": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
            "baker": 0
          }
        ],
        "outcome": "success"
      },
      "type": "delegateStake",
      "index": 1
    },
    {
      "hash": "ae13d5677cdcbd90fad54e9768f4f1f4c44610c45af1c2ca0481119d47c8a2bb",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 265,
      "energyCost": 265,
      "result": {
        "events": [
          {
            "tag": "StakeDelegated",
            "account": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
            "baker": 0
          }
        ],
        "outcome": "success"
      },
      "type": "delegateStake",
      "index": 2
    },
    {
      "hash": "73513777db8519ef5b71a0bd06a1b06bd705bf06e62737fa563ed75e20fcec27",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 3167,
      "energyCost": 3167,
      "result": {
        "events": [
          {
            "tag": "BakerAdded",
            "contents": 5
          }
        ],
        "outcome": "success"
      },
      "type": "addBaker",
      "index": 3
    },
    {
      "hash": "affc03c8211ea90cb72143e5c44eff7e55089668af8e64bd6b978ef7cfac39c7",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 265,
      "energyCost": 265,
      "result": {
        "outcome": "reject",
        "rejectReason": {
          "tag": "InvalidStakeDelegationTarget",
          "contents": 12312312312
        }
      },
      "type": "delegateStake",
      "index": 4
    },
    {
      "hash": "2b40fc10934a371e9d23dead69435d834aa725ebc418b6631abb49b5f808a07c",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 166,
      "energyCost": 166,
      "result": {
        "outcome": "reject",
        "rejectReason": {
          "tag": "SerializationFailure"
        }
      },
      "type": null,
      "index": 5
    },
    {
      "hash": "8841d4dd1da2875b63d829be55fc5441adba2efa2baaed3b1228601e25bc2bb2",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 165,
      "energyCost": 165,
      "result": {
        "outcome": "reject",
        "rejectReason": {
          "tag": "SerializationFailure"
        }
      },
      "type": null,
      "index": 6
    },
    {
      "hash": "5257c01e5f42a0afcead07149a474162c2a193702de59177bff0d7b8812a4d09",
      "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
      "cost": 165,
      "energyCost": 165,
      "result": {
        "outcome": "reject",
        "rejectReason": {
          "tag": "InvalidBakerRemoveSource",
          "contents": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt"
        }
      },
      "type": "removeBaker",
      "index": 7
    },
    {
      "hash": "2a0a5ea99e9f091a3704c7fa8d163e971f32d2a591060cdeab4a02790f7cf4ad",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 2705,
      "energyCost": 2705,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "69f895cec649178d771b016019912e84bbc659ef35e132585cb5a358b7f7ffb0"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 0
    },
    {
      "hash": "0b04e5f0e2969ea2961cc0667d48cc17166f3ad84e1cd5900d85d79e63919bd3",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 1527,
      "energyCost": 1527,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "33f8832053c36b443084040537f11a66527bc1446a0aa41704ddf06e44093455"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 1
    },
    {
      "cost": 3031,
      "energyCost": 3031,
      "hash": "0a75fdac33df28b8ae17fead28f837cb28ae23999cdb81bd5f9b7993e24cc204",
      "index": 0,
      "result": {
        "events": [
          {
            "address": {
              "index": 0,
              "subindex": 0
            },
            "amount": 0,
            "name": 200,
            "ref": "7d6b63bde558564b77070bf1da85c56e7fd9f006ef8d0923d19689577614dd2d",
            "tag": "ContractInitialized"
          }
        ],
        "outcome": "success"
      },
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "type": "initContract"
    },
    {
      "hash": "a633a418a26353989dec0e813353984892976e3c821bfac5cddee9850ce7bf9e",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 464,
      "energyCost": 464,
      "result": {
        "events": [
          {
            "amount": 10,
            "tag": "Updated",
            "instigator": {
              "address": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
              "type": "AddressAccount"
            },
            "address": {
              "subindex": 0,
              "index": 0
            },
            "message": "ExprMessage (Let (LetForeign (Imported 7d6b63bde558564b77070bf1da85c56e7fd9f006ef8d0923d19689577614dd2d (Name 130)) (Linked (Constructor (Name 130))) (Atom (BoundVar (Reference (-1))))) (Let (Constructor (Name 1)) (App (Reference 0) [BoundVar (Reference 1)])))"
          }
        ],
        "outcome": "success"
      },
      "type": "update",
      "index": 0
    }
  ]
}
"""


getBlockSummary_stub_contractDeploys =
    """
{
  "specialEvents": [
    {
      "bakerId": 3,
      "rewardAmount": 35984,
      "bakerAccount": "4EbkxwtSCopq6U1d7FcRgugR4DvKin4Wyg2qW5wmDkvWYFmDca"
    }
  ],
  "transactionSummaries": [
    {
      "hash": "2a0a5ea99e9f091a3704c7fa8d163e971f32d2a591060cdeab4a02790f7cf4ad",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 2705,
      "energyCost": 2705,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "69f895cec649178d771b016019912e84bbc659ef35e132585cb5a358b7f7ffb0"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 0
    },
    {
      "hash": "0b04e5f0e2969ea2961cc0667d48cc17166f3ad84e1cd5900d85d79e63919bd3",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 1527,
      "energyCost": 1527,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "33f8832053c36b443084040537f11a66527bc1446a0aa41704ddf06e44093455"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 1
    },
    {
      "hash": "21585869b20629a5fb13eaa40e4d488e6c458d8609e994194b6c4c0cd78ded50",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 1365,
      "energyCost": 1365,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "73310b972c101c4ec69e6eb427ebc843d5483f4adb771d1dc711f8de5ebaced4"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 2
    },
    {
      "hash": "b5fba16ba8bb959bab172beb8939700e28d043689e6fd08023034d3a2e3d5168",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 1558,
      "energyCost": 1558,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "73e1ebc954909b336a9c08443fff079e75956e7c0f314c4ad56b8bcfebaef8c4"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 3
    },
    {
      "hash": "e91402ac563df428372a23ab83a26224696d89c14d8ab1f69623f5ca05efce98",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 4890,
      "energyCost": 4890,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "3aa516e0b15327d7427b9acf7600c3e0376f65eeef7187975be38df70e0ab3e6"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 4
    },
    {
      "hash": "bac1fb11f121a88431c146acf2b5514cb723629d553f384b1c9691470b7648fe",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 657,
      "energyCost": 657,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "6a1ece5ea0d5a25bcf6e53f6e7bb4cf2b5a8cabaa8b58ba30a454e3582b26007"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 5
    },
    {
      "hash": "7df554bb85448d8cc2cc9ed11b9eac348f56105e408ffb34c987c1781cc7bace",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 1417,
      "energyCost": 1417,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "ec79b0b4ab8f91247b271af307e8d0ae7f8c6cff7bcd9ca63d214116a01aa876"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 6
    },
    {
      "hash": "404311b6f25efff2320d5091651e13745a078898e465e2bc006b4cad1c9341b0",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 3055,
      "energyCost": 3055,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "7d6b63bde558564b77070bf1da85c56e7fd9f006ef8d0923d19689577614dd2d"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 7
    },
    {
      "hash": "7c1dac909a4f49a74d80976467eecbbe641f142197747dd5e470f22167641474",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 3479,
      "energyCost": 3479,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "1e4a518694fba2bd3edecec2aaac166edf2b65b59c0c6900e26637b73a7fbb2b"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 8
    },
    {
      "hash": "4aa521cb1733ef27a78151d698a8c460c69af9f2a4d4619497b7cea73a11264f",
      "sender": "3LqzxroRTxGGBuhyoH5CzSyUvrgvhX1tPPCLVZz8QQ9ZmbrmcZ",
      "cost": 1775,
      "energyCost": 1775,
      "result": {
        "events": [
          {
            "tag": "ModuleDeployed",
            "contents": "6e33fddc0a95324af4510964827c02c87cf403255cb8f7baae90786625133a3a"
          }
        ],
        "outcome": "success"
      },
      "type": "deployModule",
      "index": 9
    }
  ]
}
  """


getBlockSummary_stub_failure =
    """
  {
    "specialEvents": [
      {
        "bakerId": 2,
        "rewardAmount": 1751,
        "bakerAccount": "3fS4u95Sx9SKzU83kxxYP4SWaWirVix4T7P9bgswmceqvcs4vR"
      }
    ],
    "transactionSummaries": [
      {
        "hash": "0ada4e64558bb99a3607647c490f39083f44c709e654d689d8bf3fb343a95961",
        "sender": "43TULx3kDPDeQy1C1iLwBbS5EEV96cYPw5ZRdA9Dm4D23hxGZt",
        "cost": 165,
        "result": {
          "outcome": "reject",
          "rejectReason": {
            "tag": "InvalidAccountReference",
            "contents": "3Es2U5gMdKrqJdXSUVzgW4KUosJ91AxfsAuvKx5tKE9P2SvjVk"
          }
        },
        "energyCost": 165,
        "type": "transfer",
        "index": 0
      }
    ]
  }
  """


getBlockSummaryResponseStub =
    """
{
  "specialEvents": [
    {
      "bakerId": 4,
      "rewardAmount": 207,
      "bakerAccount": "4c7SXWg5bD3YpWQ2mmS8DSQiFm9MRpTd7Novo4ESPb1QLSYrts"
    }
  ],
  "transactionSummaries": [
    {
      "hash": "ae3154e1ee8a64d9e9718e57fbc979175be2ee8526b23fcddab2d626ebaeb72d",
      "sender": null,
      "cost": 0,
      "result": {
        "events": [
          {
            "tag": "AccountCreated",
            "contents": "3P2Qsi8FeMB6AijMi3kpD9FrcFfAkoHDMu8kf7Fbd3cYYJpk2s"
          },
          {
            "tag": "CredentialDeployed",
            "regid": "ac64aac1da15afd90d185475705935fbe224d5f7ddc43988e7db511656c787b452820b5096a2b323cf7612f5609d4cfb",
            "account": "3P2Qsi8FeMB6AijMi3kpD9FrcFfAkoHDMu8kf7Fbd3cYYJpk2s"
          }
        ],
        "outcome": "success"
      },
      "energyCost": 35000,
      "type": null,
      "index": 0
    },
    {
      "hash": "2ecde309ea6b48cebfbb73eb1ee3364c4f10330955a4a13acb7d87076461ab2d",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "dba5e4bafc7d2201cc23d30f288e276b5305b0722a74d0ac1fe1f6b15d1496a2",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "f59897e5be69ecd5ed5843a10d74ee2b652064bd5077b853e6b324c23683af7b",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "f49899ac815b01a9f6e49c4d1fc926c511fcd3cc453c1307a504ef937a4fe8b5",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressContract"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "3ad7e0a0e3ed193bfba065c83704ae5bf84837255ac720565479afab81767c96",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressContract"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "d3b0cdeab57dc4d5e6b1faa1e4915ddfc2d025655775254e7124ae031688d9d8",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "bf6e33703977ce94caa918911f63b04654da0047454c484be9c80b487cc142c5",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "8781b249f4629f0ca9789fe143bfd4031208a7bfcca0ade9dd9f63ae8dac317d",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "41e6c23be80ab1151697ad16a4427268b0fd5440af0bb7bee4b330648669b164",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "71b1e81a467f038102b4028f62f1d75324b3f523472b89c583fdac918e762560",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "5298e7d571852da5c030c15b1513405c365111315efbc062a9d0f9082d1d5e6f",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "c7487dc3ee7cd097636badc9c21771d2a62a37052be8204078f516583cb5320e",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "dd6871001af51dc048bbae8ceb187c58fb2cf66ba838897b8f527642fb7e552a",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "fc2ee2cd1294f0dc8a0c738c25bc6f762edf2689ee37b22382d9a4b931637ae7",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "029681010cdc89383926d32017af7e51357f66b33f1527396f0af63b5b9a3e6c",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "64fbd260407f681292cc4931f8c3d7edb034574c172a90f1923188751fb6775a",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "5cc3ec13c9844456d8b65f0534dca943a4ece1b12c1ca0f8f5a2936c7e260722",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "5a9c8dd2b90b7943a4891b1ab41cbe8ae13cefb7a91c7ef3d884201408352b04",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "16672379084db2866db4564e0c42103d3f6edcdfedee6b9ce5f520b9aef78ea7",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    },
    {
      "hash": "de7eecba63d94c7810126cc325d6cf43d9382d8d2fed680202140fe97e91fe76",
      "sender": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
      "cost": 10,
      "result": {
        "events": [
          {
            "amount": 123,
            "tag": "Transferred",
            "to": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            },
            "from": {
              "address": "4KYJHs49FX7tPD2pFY2whbfZ8AjupEtX8yNSLwWMFQFUZgRobL",
              "type": "AddressAccount"
            }
          }
        ],
        "outcome": "success"
      },
      "energyCost": 10,
      "type": "transfer"
    }
  ]
}
"""